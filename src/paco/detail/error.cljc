(ns paco.detail.error
  (:refer-clojure :exclude [merge])
  (:require [clojure.string :as str]
            [paco.detail.source :as source])
  (:import #?(:clj [java.io StringWriter]
              :cljs [goog.string StringBuffer])))

(defprotocol IMessage
  "An error message."
  (-msg-type [msg]
    "Returns a keyword representing the type of this message.")
  (-write-msg! [msg writer opts]
    "Writes this message to `writer`.")
  (-compare-msgs [msg other]
    "Compare this message to `other`, determining message order."))

(defn message?
  "Returns true if `x` is a message, and optionally of the given `type`."
  ([x]
   (satisfies? IMessage x))
  ([x type]
   (and (satisfies? IMessage x)
        (= type (-msg-type x)))))

(defprotocol IError
  "An error is a collection of messages."
  (-reduce-msgs [error f result]
    "Reduce all messages in this error into `result` using the reducing
     function `f`."))

(defn- reduce-msgs [error f result]
  (cond
    (nil? error) result
    (satisfies? IMessage error) (f result error)
    (satisfies? IError error) (-reduce-msgs error f result)
    ;; Flatten collections/sequences recursively.
    :else (reduce #(reduce-msgs %2 f %1) result error)))

;; During parsing, most error messages we accumulate will be discarded,
;; once the next parser succeeds.  We therefore want to make message
;; accumulation using `merge` cheap.
(deftype Union [e1 e2]
  IError
  (-reduce-msgs [_ f result]
    (->> result (reduce-msgs e1 f) (reduce-msgs e2 f))))

(defn merge
  "Merge two errors."
  [e1 e2]
  (if (some? e1)
    (if (some? e2)
      (Union. e1 e2)
      e1)
    e2))

;; fparsec: ToHashSet
(defn message-set
  "Returns distinct error messages as a set."
  [error]
  (persistent! (reduce-msgs error conj! (transient #{}))))

(def ^:private type-priority
  (zipmap [::expected
           ::expected-input
           ::unexpected
           ::unexpected-input
           ::message
           ::nested
           ::compound
           ::other]
          (range)))

(defn- compare-types [t1 t2]
  (let [p1 (type-priority t1)
        p2 (type-priority t2)]
    (if (nil? p1)
      (if (nil? p2)
        (compare t1 t2)
        1)
      (if (nil? p2)
        -1
        (compare p1 p2)))))

(defn- compare-msgs [m1 m2]
  (let [d (compare-types (-msg-type m1) (-msg-type m2))]
    (if (zero? d)
      (-compare-msgs m1 m2)
      d)))

;; fparsec: ToSortedArray
(defn sort-messages
  "Returns a sorted sequence of distinct messages in `error`."
  [error]
  (sort compare-msgs (message-set error)))

;;---------------------------------------------------------
;; Writing

(defn- write! [writer s]
  #?(:clj  (.write ^java.io.Writer writer (str s))
     :cljs (-write writer (str s))))

(defn- write-list! [writer msgs prefix last-sep opts]
  (when-let [m (first msgs)]
    (write! writer prefix)
    (-write-msg! m writer opts)
    (when-let [ms (next msgs)]
      (doseq [m (butlast ms)]
        (write! writer ", ")
        (-write-msg! m writer opts))
      (write! writer last-sep)
      (-write-msg! (last ms) writer opts))))

(def ^:private type-group
  {::expected         ::expected
   ::expected-input   ::expected
   ::unexpected       ::unexpected
   ::unexpected-input ::unexpected
   ::message          ::message
   ::compound         ::compound
   ::nested           ::nested})

(defn- msg-group [msg]
  (type-group (-msg-type msg) ::other))

;; TODO: Options: multiline ('\n' or ';'), prefix such as "Parse Error: "
;; TODO: Pretty-printing with source line and ^ marker (take the source)
(defn write-messages!
  "Write error messages to `writer`."
  ([error writer]
   (write-messages! error writer nil))
  ([error writer pos]
   (write-messages! error writer pos nil))
  ([error writer pos opts]
   (let [{:keys [sep] :or {sep "; "}} opts
         {::keys [expected unexpected message nested compound _other]
          :as grouped}
         (group-by msg-group (sort-messages error))]
     (write-list! writer expected "expected " " or " opts)
     ;; TODO: Use a custom Writer that can track writes,
     ;; so that we can do (maybe-newline)
     (when (seq unexpected)
       (when (seq expected)
         (write! writer sep))
       (write-list! writer unexpected "unexpected " " and " opts))
     (when (seq message)
       (when (or (seq expected) (seq unexpected))
         (write! writer sep))
       (write! writer "Other error messages: ")
       (doseq [msg message]
         (-write-msg! msg writer opts)))
     (doseq [msg compound]
       (write! writer sep)
       (-write-msg! msg writer opts))
     ;; ? other messages
     (when (empty? grouped)
       (write! writer "Unknown error(s)"))
     (doseq [msg nested]
       (write! writer sep)
       (-write-msg! msg writer opts))
     (when pos
       (write! writer " at line ")
       (write! writer (inc (source/pos-line pos)))
       (write! writer ", column ")
       (write! writer (inc (source/pos-col pos)))))))

(defn string
  "Returns a string representation of `error`."
  ([error]
   (string error nil))
  ([error pos]
   #?(:clj (with-open [writer (StringWriter.)]
             (write-messages! error writer pos)
             (.flush writer)
             (str writer))
      :cljs (let [sb (StringBuffer.)
                  writer (StringBufferWriter. sb)]
              (write-messages! error writer pos)
              (-flush writer)
              (str sb)))))

(defn render
  "Default error renderer."
  [source error]
  (string error (source/position source)))

;;---------------------------------------------------------
;; Messages

(defrecord Label [type label]
  IMessage
  (-msg-type [_] type)
  (-write-msg! [_ writer _opts] (write! writer label))
  (-compare-msgs [_ other]
    (compare label (.-label ^Label other))))

(defrecord Input [type input]
  IMessage
  (-msg-type [_] type)
  (-write-msg! [_ writer _opts]
    (let [quote (if (str/includes? input "'") "\"" "'")]
      (write! writer quote)
      (write! writer input)
      (write! writer quote)))
  (-compare-msgs [_ other]
    (compare input (.-input ^Input other))))

(defn- compare-positions [pos1 pos2]
  (let [d (#?(:clj Long/compare :default compare) (source/pos-line pos1) (source/pos-line pos2))]
    (if (zero? d)
      (#?(:clj Long/compare :default compare) (source/pos-col pos1) (source/pos-col pos2))
      d)))

;; alternative name: Backtracked
(defrecord Nested [type label pos error]
  IMessage
  (-msg-type [_] type)
  (-write-msg! [_ writer opts]
    (case type
      ::nested   (write! writer "backtracked after: ")
      ::compound (do
                   (write! writer label)
                   (write! writer " could not be parsed because: ")))
    (write-messages! error writer pos opts))
  (-compare-msgs [_ other]
    (let [dpos (compare-positions pos (.-pos ^Nested other))]
      (if (zero? dpos)
        (let [dlabel (compare label (.-label ^Nested other))]
          (if (zero? dlabel)
            (compare (sort-messages error)
                     (sort-messages (.-error ^Nested other)))
            dlabel))
        dpos))))

(defn expected
  "Returns an error message indicating that the input did not match an
   expected pattern, described by `label`."
  [label]
  (->Label ::expected (str label)))

;; fparsec: + ExpectedCaseInsensitiveString
(defn expected-input
  "Returns an error message indicating that the input did not match the
   expected `input`."
  [input]
  (->Input ::expected-input (str input)))

(defn unexpected
  "Returns an error message indicating that the parser encountered some
   unexpected input, described by `label`."
  [label]
  (->Label ::unexpected (str label)))

;; fparsec: + UnexpectedCaseInsensitiveString
(defn unexpected-input
  "Returns an error message indicating that `input` the parser encountered
   was unexpected."
  [input]
  (->Input ::unexpected-input (str input)))

(defn message
  "Returns a generic error message that does not fit the other types."
  [text]
  (->Label ::message (str text)))

;; or: backtracked
(defn nested
  "Returns an error message indicating that the parser backtracked after it
   encountered `error`."
  [source error]
  ;; Don't wrap backtrack messages.
  (if (message? error ::nested)
    error
    (->Nested ::nested nil (source/position source) error)))

(defn nested->compound
  "Turns a `nested` error to a `compound` one."
  [error label]
  (assoc error :type ::compound, :label label))

;; backtrack + label
(defn compound
  "Returns a compound error message, generated by the compound-labelling
   operator."
  [label source error]
  (if (message? error ::nested)
    (nested->compound error label)
    (->Nested ::compound (str label) (source/position source) error)))

;; fparsec: + Other

(def ^:private end-label "end of input")

(def unexpected-end
  "This error indicates that the parser encountered an unexpected end of
   the input stream."
  (unexpected end-label))

(def expected-end
  "This error indicates that the parser expected the end of the input stream,
   but encountered a different token."
  (expected end-label))

(def no-message
  "An error without message."
  nil)

(defn unexpected-token-or-end
  "Returns an error indicating that the next token or end of input was
   unexpected."
  ([source]
   (if (source/end? source)
     unexpected-end
     (unexpected-input (source/peek source))))
  ([source error]
   (merge error (unexpected-token-or-end source))))
