(ns paco.core
  (:refer-clojure :exclude [* + cat max min not-empty ref repeat sequence])
  (:require [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.parsers :as dp]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.scanner :as scanner])
  #?(:cljs (:require-macros [paco.core])))

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
(defn pipe
  ([p f]
   (dp/with-seq [x p] (f x)))
  ([p1 p2 f]
   (dp/with-seq [x p1, y p2] (f x y)))
  ([p1 p2 p3 f]
   (dp/with-seq [x p1, y p2, z p3] (f x y z)))
  ([p1 p2 p3 p4 & more]
   (let [ps (list* p1 p2 p3 p4 (butlast more))
         f  (last more)]
     (dp/pseq ps (completing conj #(apply f %))))))

;; alternative names: pseq, sq, tup (tuple), grp (group), group-all
(defn sequence
  "Applies the parsers `ps` in sequence and returns a collection of
   their return values."
  [ps]
  (dp/pseq ps))

;; fparsec: .>>., tuple2, tuple3, tuple4, tuple5
;; alternative names: tuple
(defn group
  ([p]
   (pipe p vector))
  ([p1 p2]
   (pipe p1 p2 vector))
  ([p1 p2 p3]
   (pipe p1 p2 p3 vector))
  ([p1 p2 p3 p4 & more]
   (sequence (list* p1 p2 p3 p4 more))))

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
   (dp/pseq (list* p1 p2 p3 p4 more) rfs/rlast)))

;; fparsec: .>>
(defn then-skip
  "Applies the parsers in sequence and returns the result of the first one."
  ([p] p)
  ([p1 p2]
   (dp/with-seq [x p1, _ p2] x))
  ([p1 p2 p3]
   (dp/with-seq [x p1, _ p2, _ p3] x))
  ([p1 p2 p3 p4 & more]
   (dp/pseq (list* p1 p2 p3 p4 more) rfs/rfirst)))

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
;; alternate name: label
(defn as
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
(defn as! [p label]
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
(defn attempt [p]
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

;; not to be confused with regex negative look-ahead (?!...)
;; alternate names: ?!, ?attempt, ?try
(defn ?attempt
  "Applies `p`.  If `p` fails, `?!` will backtrack to the original state
   and succeed with `not-found` or `nil`.

   `(?attempt p)` is an optimized implementation of `(? (attempt p))`."
  ([p] (?attempt p nil))
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
;; `attempt` could be: `(catch p #(fail-with error))`
;; alternate names: except
#_(defn catch
    "When `p` fails, backtracks and resumes with the parser returned by
   `(f error)`."
    [p f])

;;---------------------------------------------------------
;; Parsing alternatives

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
  ([ps label]
   (if-let [p (first ps)]
     (as (reduce #(alt2 %1 %2 false) p (next ps)) label)
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
;; names: ?=, ?!, ?<=, ?<!
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

(defn cats [ps]
  (dp/pseq ps rfs/seqex))

(defn cat [& ps]
  (dp/pseq ps rfs/seqex))

;; alternative names: opt, maybe
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
  (dp/sep `*sep-by p sep rfs/rvec true false))

(defn +sep-by [p sep]
  (dp/sep `+sep-by p sep rfs/rvec false false))

(defn *skip-sep-by [p sep]
  (dp/sep `*skip-sep-by p sep rfs/ignore true false))

(defn +skip-sep-by [p sep]
  (dp/sep `+skip-sep-by p sep rfs/ignore false false))

(defn *sep-end-by [p sep]
  (dp/sep `*sep-end-by p sep rfs/rvec true true))

(defn +sep-end-by [p sep]
  (dp/sep `+sep-end-by p sep rfs/rvec false true))

(defn *skip-sep-end-by [p sep]
  (dp/sep `*skip-sep-end-by p sep rfs/ignore true true))

(defn +skip-sep-end-by [p sep]
  (dp/sep `+skip-sep-end-by p sep rfs/ignore false true))

;; fparsec: manyTill + variants

(defn *until [p endp]
  (dp/until `*until p endp rfs/rvec true false))

(defn +until [p endp]
  (dp/until `+until p endp rfs/rvec false false))

;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Lazy / recursive

(defn pforce
  "Forces the delay `d` on-demand, which should yield a parser
   that is applied in its stead."
  [d]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [p (force d)]
        (parser/apply p scanner reply)))
    (children [_] [(force d)])))

;; alternate name: pdelay
(defmacro lazy [& body]
  `(pforce (delay ~@body)))

;; fparsec: createParserForwardedToRef
;; alternate name: pderef
(defn ref
  "Returns a parser that forwards all calls to the parser returned
   by `@ref`.  Useful to construct recursive parsers."
  [ref]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [p @ref]
        (parser/apply p scanner reply)))
    (children [_] [@ref])))

(defn rec
  "Creates a recursive parser.  Calls `f` with one arg, a parser to recur,
   and returns the parser returned by `f`.

   It is assumed that `f` uses `p/alt` or similar to eventually stop the
   recursion."
  [f]
  (let [vol (volatile! pnil)]
    (vreset! vol (f (ref vol)))))

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

(defn match-user-state
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
