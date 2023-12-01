(ns paco.error
  (:refer-clojure :exclude [merge])
  (:require [clojure.string :as str]
            [paco.state :as state])
  (:import #?(:clj [java.io StringWriter]
              :cljs [goog.string StringBuffer])))

(defprotocol IError
  (-reduce-msgs [error f result]))

(defprotocol IMessage
  (-type [msg])
  (-write-msg! [msg writer opts])
  (-msg-compare [msg other]))

(defn message?
  ([x]
   (satisfies? IMessage x))
  ([x type]
   (and (satisfies? IMessage x)
        (= type (-type x)))))

(defn- reduce-msgs [error f result]
  (cond
    ;; TODO: Filter empty Label and Input messages?
    (satisfies? IMessage error) (f result error)
    (satisfies? IError error) (-reduce-msgs error f result)
    ;; Flatten sequences recursively.
    :else (reduce #(reduce-msgs %2 f %1) result error)))

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

;; fparsec: ToSortedArray
(defn sort-messages
  "Returns a sorted sequence of error messages."
  [error]
  (sort -msg-compare (message-set error)))

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
  {::expected ::expected
   ::expected-input   ::expected
   ::unexpected ::unexpected
   ::unexpected-input ::unexpected
   ::nested ::nested
   ::compound ::compound})

(defn- msg-group [msg]
  (type-group (-type msg) ::other))

;; TODO: Options: multiline ('\n' or ';'), prefix such as "Parse Error: "
;; TODO: Pretty-printing with source line and ^ marker
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
       (write! writer sep)
       (write! writer "Other error messages: ")
       (doseq [msg message]
         (-write-msg! msg writer opts)))
     (doseq [msg compound]
       (write! writer sep)
       (-write-msg! msg writer opts))
     (when (empty? grouped)
       (write! writer "Unknown error(s)"))
     (doseq [msg nested]
       (write! writer sep)
       (-write-msg! msg writer opts))
     (when pos
       (write! writer " at ")
       (write! writer pos)))))

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

;;---------------------------------------------------------
;; Messages

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

(defrecord Label [type label]
  IMessage
  (-type [_] type)
  (-write-msg! [_ writer _opts] (write! writer label))
  (-msg-compare [_ other]
    (let [d (compare-types type (-type other))]
      (if (zero? d)
        (compare label (.-label ^Label other))
        d))))

(defrecord Input [type input]
  IMessage
  (-type [_] type)
  (-write-msg! [_ writer _opts]
    (let [quote (if (str/includes? input "'") "\"" "'")]
      (write! writer quote)
      (write! writer input)
      (write! writer quote)))
  (-msg-compare [_ other]
    (let [d (compare-types type (-type other))]
      (if (zero? d)
        (compare input (.-input ^Input other))
        d))))

;; alternative name: backtracked
(defrecord Nested [type label pos error]
  IMessage
  (-type [_] type)
  (-write-msg! [_ writer opts]
    (case type
      ::nested   (write! writer "backtracked after: ")
      ::compound (do
                   (write! writer label)
                   (write! writer " could not be parsed because: ")))
    (write-messages! error writer pos opts))
  (-msg-compare [_ other]
    (let [d (compare-types type (-type other))]
      (if (zero? d)
        (let [d (compare pos (.-pos ^Nested other))]
          (if (zero? d)
            (let [d (compare label (.-label ^Nested other))]
              (if (zero? d)
                (compare (sort-messages error)
                         (sort-messages (.-error ^Nested other)))
                d))
            d))
        d))))

(defn expected
  "The input does not match the expected input.

   `label` describes the expected input."
  [label]
  (->Label ::expected (str label)))

;; fparsec: + ExpectedCaseInsensitiveString
(defn expected-input
  "The input does not match an expected string constant."
  [input]
  (->Input ::expected-input (str input)))

(defn unexpected
  "The parser encountered some unexpected input."
  [label]
  (->Label ::unexpected (str label)))

;; fparsec: + UnexpectedCaseInsensitiveString
(defn unexpected-input
  "The parser encountered some unexpected input."
  [input]
  (->Input ::unexpected-input (str input)))

(defn message
  "The error does not fit the other types."
  [text]
  (->Label ::message (str text)))

(defn nested
  "Backtracked after an error occurred."
  [state error]
  (->Nested ::nested nil (state/pos state) error))

(defn compound
  "Mainly generated by compound-labelling operator."
  [label state error]
  (->Nested ::compound (str label) (state/pos state) error))

;; fparsec: + Other

(def ^:private eof-label "end of input")

(def unexpected-eof (unexpected eof-label))
(def expected-eof (expected eof-label))

(def no-message nil)

(comment
  (string unexpected-eof)
  (string [(expected "something better")
           unexpected-eof])
  (string (merge (expected "something better")
                 (merge (unexpected-input "x")
                        (merge
                         (unexpected-input \x)
                         unexpected-eof))))

;;
  )
