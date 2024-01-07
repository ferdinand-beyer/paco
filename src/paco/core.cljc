(ns paco.core
  (:refer-clojure :exclude [* + cat deref map max min not-empty ref repeat sequence])
  (:require [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.parsers :as dp]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.scanner :as scanner])
  #?(:cljs (:require-macros [paco.core :refer [fwd]])))

(defn- result-data [scanner reply]
  {:ok?        (reply/ok? reply)
   :value      (reply/value reply)
   :error      (reply/error reply)
   :index      (scanner/index scanner)
   :position   (scanner/position scanner)
   :user-state (scanner/user-state scanner)})

(defn run [p input & {:as opts}]
  (let [scanner (scanner/of input opts)
        reply   (parser/apply p scanner (reply/mutable-reply))]
    (result-data scanner reply)))

(defn- parse-exception [scanner reply]
  (let [{:keys [error position] :as data} (result-data scanner reply)]
    (ex-info (error/string error position)
             (assoc data :type ::parse-error))))

(defn parse [p input & {:as opts}]
  (let [scanner (scanner/of input opts)
        reply   (parser/apply p scanner (reply/mutable-reply))]
    (if (reply/ok? reply)
      (reply/value reply)
      (throw (parse-exception scanner reply)))))

;;---------------------------------------------------------
;; Basic parsers

;; fparsec: preturn, >>%
(defn return
  ([x]
   (fn [_scanner reply]
     (reply/ok reply x)))
  ([p x]
   (reify parser/IParser
     (apply [_ scanner reply]
       (reply/with-value (parser/apply p scanner reply) x))
     (children [_] [p]))))

;; fparsec: pzero, but fails
(def pnil
  "This parser always succeeds and returns `nil`."
  (return nil))

(defn end
  "This parser succeeds a the end of the input stream."
  [scanner reply]
  (if (scanner/end? scanner)
    (reply/ok reply nil)
    (reply/fail reply error/expected-end)))

(defn any-token [scanner reply]
  (if-some [token (scanner/peek scanner)]
    (do
      (scanner/skip! scanner)
      (reply/ok reply token))
    (reply/fail reply (error/unexpected-token-or-end scanner))))

(defn skip-any-token [scanner reply]
  (if (pos? (scanner/skip! scanner))
    (reply/ok reply nil)
    (reply/fail reply (error/unexpected-token-or-end scanner))))

(defn token-return [token value]
  (let [expected (error/expected-input token)]
    (fn [scanner reply]
      (if (= token (scanner/peek scanner))
        (do
          (scanner/skip! scanner)
          (reply/ok reply value))
        (reply/fail reply (error/merge expected (error/unexpected-token-or-end scanner)))))))

(defn token [token]
  (token-return token token))

;;---------------------------------------------------------
;; Chaining and piping

(defn bind [p f]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [reply (parser/apply p scanner reply)]
        (if (reply/ok? reply)
          (let [p2 (f (reply/value reply))]
            (if-some [error (reply/error reply)]
              (let [modcount (scanner/modcount scanner)
                    reply    (parser/apply p2 scanner reply)]
                (if (= modcount (scanner/modcount scanner))
                  (reply/with-error reply (error/merge error (reply/error reply)))
                  reply))
              (parser/apply p2 scanner reply)))
          reply)))
    (children [_] [p])))

(defn- emit-with [bindings body]
  (let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-with (drop 2 bindings) body))))))

;; ? rename to `let`?
;; alternative names: let-bind,  plet
(defmacro with
  {:clj-kondo/lint-as 'clojure.core/let
   :style/indent 1}
  [bindings & body]
  {:pre [(vector? bindings)
         (even? (count bindings))
         (some? body)]}
  (emit-with bindings body))

;; fparsec: |>>, pipe2, pipe3, pipe4, pipe5
;; alternative names: map, fmap, pmap
(defn map
  ([p f]
   (dp/with-seq [x p] (f x)))
  ([p1 p2 f]
   (dp/with-seq [x p1, y p2] (f x y)))
  ([p1 p2 p3 f]
   (dp/with-seq [x p1, y p2, z p3] (f x y z)))
  ([p1 p2 p3 p4 & more]
   (let [ps (list* p1 p2 p3 p4 (butlast more))
         f  (last more)]
     (dp/reduce ps (completing conj #(apply f %))))))

(defn tuple*
  "Applies the parsers `ps` in sequence and returns a vector of
   their return values."
  [ps]
  (dp/reduce ps rfs/vector))

;; fparsec: .>>., tuple2, tuple3, tuple4, tuple5
(defn tuple
  ([p]
   (map p vector))
  ([p1 p2]
   (map p1 p2 vector))
  ([p1 p2 p3]
   (map p1 p2 p3 vector))
  ([p1 p2 p3 p4 & more]
   (tuple* (list* p1 p2 p3 p4 more))))

;; fparsec: >>.
;; parsesso: after
(defn then
  "Applies the parsers in sequence and returns the result of the last one."
  ([p] p)
  ([p1 p2]
   (dp/with-seq [_ p1, x p2] x))
  ([p1 p2 p3]
   (dp/with-seq [_ p1, _ p2, x p3] x))
  ([p1 p2 p3 p4 & more]
   (dp/reduce (list* p1 p2 p3 p4 more) rfs/last)))

;; fparsec: .>>
(defn then-skip
  "Applies the parsers in sequence and returns the result of the first one."
  ([p] p)
  ([p1 p2]
   (dp/with-seq [x p1, _ p2] x))
  ([p1 p2 p3]
   (dp/with-seq [x p1, _ p2, _ p3] x))
  ([p1 p2 p3 p4 & more]
   (dp/reduce (list* p1 p2 p3 p4 more) rfs/first)))

;; fparsetc: .>>.: like (cat p1 p2)

(defn between
  ([p psurround]
   (between p psurround psurround))
  ([p popen pclose]
   (dp/with-seq [_ popen, x p, _ pclose] x)))

;;---------------------------------------------------------
;; Customizing and recovering from errors

(defn fail [message]
  (let [error (error/message message)]
    (fn [_scanner reply]
      (reply/fail reply error))))

;; fparsec: <?>
(defn label
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (let [expected-error (error/expected label)]
    (reify parser/IParser
      (apply [_ scanner reply]
        (let [modcount (scanner/modcount scanner)
              reply    (parser/apply p scanner reply)]
          (if (= modcount (scanner/modcount scanner))
            (reply/with-error reply expected-error)
            reply)))
      (children [_] [p]))))

;; fparsec: <??>
(defn label-compound
  [p label]
  (let [expected-error (error/expected label)]
    (reify parser/IParser
      (apply [_ scanner reply]
        (let [state (scanner/state scanner)
              reply (parser/apply p scanner reply)]
          (if (reply/ok? reply)
            (if (scanner/in-state? scanner state)
              (reply/with-error reply expected-error)
              reply)
            (let [error (reply/error reply)]
              (if (scanner/in-state? scanner state)
                (reply/with-error reply (if (error/message? error ::error/nested)
                                          (error/nested->compound error label)
                                          expected-error))
                (do
                  ;; Backtrack, but keep mark the scanner as modified, so that
                  ;; normal parsing does not continue.
                  (scanner/backtrack-modified! scanner state)
                  (reply/with-error reply (error/compound label scanner error))))))))
      (children [_] [p]))))

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover, !, atomic, unit
;; TODO: Add an arity (atomic p label) for (atomic (labelc p label))
(defn atomic
  "Returns a parser that behaves like `p`, except that it backtracks to
   the original parser state when `p` fails with changing the parser
   state."
  [p]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [state (scanner/state scanner)
            reply (parser/apply p scanner reply)]
        (if (or (reply/ok? reply) (scanner/in-state? scanner state))
          reply
          (let [error (error/nested scanner (reply/error reply))]
            (scanner/backtrack! scanner state)
            (reply/fail reply error)))))
    (children [_] [p])))

(defn ?atomic
  "Returns a parser that applies `p` and if `p` fails, backtracks to the
   original state and _succeeds_ with value `not-found` (default: `nil`).

   `(?atomic p)` is an optimized implementation of `(? (atomic p))`."
  ([p] (?atomic p nil))
  ([p not-found]
   (reify parser/IParser
     (apply [_ scanner reply]
       (let [state (scanner/state scanner)
             reply (parser/apply p scanner reply)]
         (if (reply/ok? reply)
           reply
           (if (scanner/in-state? scanner state)
             (reply/ok reply not-found (reply/error reply))
             (let [error (error/nested scanner (reply/error reply))]
               (scanner/backtrack! scanner state)
               (reply/ok reply not-found error))))))
     (children [_] [p]))))

;; TODO
;; Maybe two variants: All errors and only when changed state (`catch` and `catch!`)
;; on-error: (f scanner reply original-state) => detail
;; variant: on-error-apply: backtrack and fall back to another one?
;; alternate names: except
#_(defn catch
    "When `p` fails, backtracks and resumes with the parser returned by
   `(f error)`."
    [p f])

;;---------------------------------------------------------
;; Parsing alternatives

;; ? Move to detail.parsers?
(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 true))
  ([p1 p2 merge-errors?]
   (reify parser/IParser
     (apply [_ scanner reply]
       (let [modcount (scanner/modcount scanner)
             reply    (parser/apply p1 scanner reply)]
         (if (or (reply/ok? reply)
                 (not= modcount (scanner/modcount scanner)))
           reply
           (let [error (reply/error reply)
                 reply (parser/apply p2 scanner reply)]
             (if (and merge-errors? (= modcount (scanner/modcount scanner)))
               (reply/with-error reply (error/merge error (reply/error reply)))
               reply)))))
     (children [_] [p1 p2]))))

;; fparsec: <|>
(defn alt
  ([p1] p1)
  ([p1 p2]
   (alt2 p1 p2))
  ([p1 p2 p3]
   (-> p1 (alt2 p2) (alt2 p3)))
  ([p1 p2 p3 & more]
   (reduce alt2 p1 (list* p2 p3 more))))

;; fparsec: choice, choiceL
;; "choiceL is slightly faster than choice, because it doesn’t
;; have to aggregate error messages."
(defn alts
  ([ps]
   (if-let [p (first ps)]
     (reduce alt2 p (next ps))
     pnil))
  ([ps label']
   (if-let [p (first ps)]
     (label (reduce #(alt2 %1 %2 false) p (next ps)) label')
     pnil)))

;;---------------------------------------------------------
;; Conditional parsing and looking ahead

;; ? Pass `label`?
(defn not-empty
  "Like `p`, but fails when `p` does not change the parser state."
  [p]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [modcount (scanner/modcount scanner)
            reply    (parser/apply p scanner reply)]
        (if (= modcount (scanner/modcount scanner))
          (reply/with-ok reply false)
          reply)))
    (children [_] [p])))

;; fparsec: followedBy, followedByL
;; alternative names: follows, assert-next
(defn followed-by
  ([p]
   (followed-by p nil))
  ([p label]
   (let [expected-error (some-> label error/expected)]
     (reify parser/IParser
       (apply [_ scanner reply]
         (let [state (scanner/state scanner)
               reply (parser/apply p scanner reply)]
           ;; TODO: Benchmark if we need the conditional
           (when-not (scanner/in-state? scanner state)
             (scanner/backtrack! scanner state))
           (if (reply/ok? reply)
             (reply/ok reply nil)
             (reply/fail reply expected-error))))
       (children [_] [p])))))

;; fparsec: notFollowedBy, notFollowedByL
;; alternative names: assert-not-next
(defn not-followed-by
  ([p]
   (not-followed-by p nil))
  ([p label]
   (let [expected-error (some-> label error/unexpected)]
     (reify parser/IParser
       (apply [_ scanner reply]
         (let [state (scanner/state scanner)
               reply (parser/apply p scanner reply)]
           ;; TODO: Benchmark if we need the conditional
           (when-not (scanner/in-state? scanner state)
             (scanner/backtrack! scanner state))
           (if (reply/ok? reply)
             (reply/fail reply expected-error)
             (reply/ok reply nil))))
       (children [_] [p])))))

;; fparsec: lookAhead
;; alternate names: peek; ?=, ?!, ?<=, ?<!
(defn look-ahead
  [p]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [state (scanner/state scanner)
            reply (parser/apply p scanner reply)]
        (if (reply/ok? reply)
          (do
            ;; TODO: Benchmark if we need the conditional
            (when-not (scanner/in-state? scanner state)
              (scanner/backtrack! scanner state))
            (reply/with-error reply nil))
          (if (scanner/in-state? scanner state)
            reply
            (let [error (error/nested scanner (reply/error reply))]
              (scanner/backtrack! scanner state)
              (reply/fail reply error))))))
    (children [_] [p])))

;;---------------------------------------------------------
;; Sequences / seqexp

;;? Add a fn to treat a sequex as one unit to prevent flattening?

(defn cat* [ps]
  (dp/reduce ps rfs/seqex))

(defn cat [& ps]
  (dp/reduce ps rfs/seqex))

;; alternative names: opt
(defn ?
  "Optional: zero or one."
  ([p]
   (alt2 p pnil))
  ([p not-found]
   (alt2 p (return not-found))))

(defn *
  "Zero or more."
  [p]
  (dp/repeat-many `* p rfs/seqex))

(defn +
  "One or more."
  [p]
  (dp/repeat-min `+ p rfs/seqex 1))

;; fparsec: skipMany
(defn *skip [p]
  (dp/repeat-many `*skip p rfs/ignore))

;; fparsec: skipMany1
(defn +skip [p]
  (dp/repeat-min `+skip p rfs/ignore 1))

;; fparsec: parray, +skipArray
(defn repeat
  ([p n]
   (dp/repeat-times `repeat p rfs/seqex n))
  ([p min max]
   (dp/repeat-min-max `repeat p rfs/seqex min max)))

(defn min [p n]
  (dp/repeat-min `min p rfs/seqex n))

(defn max [p n]
  (dp/repeat-max `max p rfs/seqex n))

(defn *sep-by [p sep]
  (dp/sep `*sep-by p sep rfs/vector true false))

(defn +sep-by [p sep]
  (dp/sep `+sep-by p sep rfs/vector false false))

(defn *skip-sep-by [p sep]
  (dp/sep `*skip-sep-by p sep rfs/ignore true false))

(defn +skip-sep-by [p sep]
  (dp/sep `+skip-sep-by p sep rfs/ignore false false))

(defn *sep-end-by [p sep]
  (dp/sep `*sep-end-by p sep rfs/vector true true))

(defn +sep-end-by [p sep]
  (dp/sep `+sep-end-by p sep rfs/vector false true))

(defn *skip-sep-end-by [p sep]
  (dp/sep `*skip-sep-end-by p sep rfs/ignore true true))

(defn +skip-sep-end-by [p sep]
  (dp/sep `+skip-sep-end-by p sep rfs/ignore false true))

;; fparsec: manyTill + variants

(defn *until [p endp]
  (dp/until `*until p endp rfs/vector true false))

(defn +until [p endp]
  (dp/until `+until p endp rfs/vector false false))

;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Forwarding / lazy / recursive

(defmacro fwd
  "Returns a parser that forwards to `expr`.  Useful for recursive parsers
   to use a parser that has been declared but not yet defined:

   ```clojure
   (def expr (p/alt c/digit
                    (p/between (p/fwd expr) (c/char \\() (c/char \\)))))
   ```

   **Warning**: Left-recursive parsers are not supported and will lead to
   infinite loops / stack overflows."
  [expr]
  `(reify parser/IParser
     (~'apply [~'_ scanner# reply#]
       (parser/apply ~expr scanner# reply#))
     (~'children [~'_] [~expr])))

(defn pforce
  "Returns a parser that forwards to ``."
  [d]
  (fwd (force d)))

;; alternate name: pdelay, delay, defer
(defmacro lazy [& body]
  `(pforce (delay ~@body)))

;; fparsec: createParserForwardedToRef
(defn deref
  "Returns a parser that forwards all calls to the parser returned
   by `@ref`.  Useful to construct recursive parsers."
  [ref]
  (fwd @ref))

(defn rec
  "Creates a recursive parser.  Calls `f` with one arg, a parser to recur,
   and returns the parser returned by `f`.

   It is assumed that `f` uses `p/alt` or similar to eventually stop the
   recursion."
  [f]
  (let [vol (volatile! pnil)]
    (vreset! vol (f (deref vol)))))

;;---------------------------------------------------------
;; Handling state

(defn index
  "Returns the index of the next token in the input stream."
  [scanner reply]
  (reply/ok reply (scanner/index scanner)))

;; TODO: This assumes an underlying char stream?
(defn pos
  "Returns the current position in the input stream."
  [scanner reply]
  (reply/ok reply (scanner/position scanner)))

(defn user-state
  "Returns the current user state."
  [scanner reply]
  (reply/ok reply (scanner/user-state scanner)))

(defn set-user-state
  "Sets the user scanner to `u`."
  [u]
  (fn [scanner reply]
    (scanner/with-user-state! scanner u)
    (reply/ok reply u)))

(defn swap-user-state
  "Sets ths user scanner to `(apply f user-state args)`."
  [f & args]
  (fn [scanner reply]
    (let [u (apply f (scanner/user-state scanner) args)]
      (scanner/with-user-state! scanner u)
      (reply/ok reply u))))

(defn some-user-state
  "Succeeds if `pred` returns logical true when called with the current
   user scanner, otherwise it fails.  Returns the return value of `pred`."
  [pred]
  (fn [scanner reply]
    (if-let [ret (pred (scanner/user-state scanner))]
      (reply/ok reply ret)
      (reply/fail reply error/no-message))))

;;---------------------------------------------------------
;; Nesting parsers

;; TODO: Move this to a separate namespace?

;; Multiple use cases:
;; - Generate a scanner that repeatedly applies a parser to obtain
;;   tokens (lexer).  Higher-level parsers can then be defined on
;;   these tokens (instead of characters).
;; - Parse the string result value of a parser:
;;   - String interpolation
;;   - HTML attributes with sub-grammar
;;   - Markdown inlines
;;   - Doc comments
;; - Generate a scanner that concatenates strings returned from an
;;   underlying parser.  Similar to the examples above, but the
;;   chunks of strings are not considered tokens (the individual
;;   characters are), and don't need to be buffered into a single
;;   string before parsing.

;; TODO: Track `index`
;; Maybe: cache indexes of newlines seen, so that we can
;; efficiently translate between index and line/col?
#_(defrecord Token [value pos])

#_(defn token
    "Behaves like `p`, but wraps its return value in a `Token` record."
    [p]
    (with [pos pos, val p]
      (return (Token. val pos))))

#_(defn tokenizer
    "Tokenizes the input stream using the parser `p`, skipping over tokens
   matching `skip-p` (e.g. whitespace).  Returns a sequence of `Token`
   records."
    [p skip-p]
    (then (? skip-p) (sep-end-by* (token p) skip-p)))

#_(defn embed
    "Parses the return value of `p` using the `inner-p` parser."
    [p inner-p]
    (fn [state reply]
      (detail/call p state
                   (fn [status1 state1 value1 error1]
                     (if (detail/ok? status1)
                     ;; TODO: How to share position information between states?
                     ;; TODO: Share user state?
                       (detail/call inner-p (state/of value1 nil)
                                    (fn [status2 state2 value2 error2]
                                    ;; TODO: How do we combine errors?
                                    ;; Probably compare state indexes
                                      (if (detail/ok? status2)
                                        (reply :ok state1 value2 nil)
                                        (reply status2 state2 nil error2))))
                       (reply status1 state1 value1 error1))))))
