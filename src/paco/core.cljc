(ns paco.core
  (:refer-clojure :exclude [* + cat max min not-empty ref repeat sequence])
  (:require [paco.detail :as detail]
            [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.parsers :as dp]
            [paco.detail.reply :as reply]
            [paco.detail.scanner :as scanner]
            [paco.state :as state]))

(defn- result-data [scanner reply]
  {:status     (reply/status reply)
   :value      (reply/value reply)
   :error      (reply/error reply)
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

(defn any-token [scanner reply]
  (if-some [token (scanner/peek scanner)]
    (do
      (scanner/skip! scanner)
      (reply/ok reply token))
    (reply/fail reply (scanner/unexpected-error scanner))))

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
                  (reply/with-error reply (error/merge error (reply/error error)))
                  reply))
              (parser/apply p2 scanner reply)))
          reply)))
    (children [_] [p])))

(defn- emit-with [bindings body]
  (let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      ;; ? emit "then" body?
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-with (drop 2 bindings) body))))))

;; ? rename to `let`?
;; alternative names: let-then, let-chain, let-seq, plet
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
     (dp/pseq ps (reply/collector (completing conj #(apply f %)))))))

;; alternative names: pseq, sq, tup (tuple), grp (group), group-all
(defn sequence
  "Applies the parsers `ps` in sequence and returns a collection of
   their return values."
  [ps]
  (dp/pseq ps))

;; fparsec: .>>., tuple2, tuple3, tuple4, tuple5
;; alternative names: tuple
(defn group
  ([p] p)
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
   (dp/with-seq [x p1, _ p2] x))
  ([p1 p2 p3]
   (dp/with-seq [x p1, _ p2, _ p3] x))
  ([p1 p2 p3 p4 & more]
   (dp/pseq (list* p1 p2 p3 p4 more) reply/first-collector)))

;; fparsec: .>>
(defn then-skip
  "Applies the parsers in sequence and returns the result of the first one."
  ([p] p)
  ([p1 p2]
   (dp/with-seq [_ p1, x p2] x))
  ([p1 p2 p3]
   (dp/with-seq [_ p1, _ p2, x p3] x))
  ([p1 p2 p3 p4 & more]
   (dp/pseq (list* p1 p2 p3 p4 more) reply/last-collector)))

;; fparsetc: .>>.: like (cat p1 p2)

(defn between
  ([p psurround]
   (between p psurround psurround))
  ([p popen pclose]
   (dp/with-seq [_ popen, x p, _ pclose] x)))

;;---------------------------------------------------------
;; Parsing alternatives and recovering from errors

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover, !
(defn attempt [p]
  (fn [state reply]
    (detail/call p state (fn [status1 state1 value1 error1]
                           (cond
                             (detail/ok? status1)
                             (reply status1 state1 value1 error1)

                             (not (detail/same-state? state1 state))
                             (reply :error state nil (error/nested state1 error1))

                             (detail/fatal? status1) (reply :error state1 value1 error1)

                             :else (reply status1 state1 value1 error1))))))

;; not to be confused with regex negative look-ahead (?!...)
;; alternate names: ?!, ?attempt, ?try
(defn ?!
  "Applies `p`.  If `p` fails, `?!` will backtrack to the original state
   and succeed with `not-found` or `nil`.

   `(?! p)` is an optimized implementation of `(? (attempt p))`."
  ([p] (?! p nil))
  ([p not-found]
   (fn [state reply]
     (detail/call p state (fn [status1 state1 value1 error1]
                            (if (detail/fail? status1)
                              (reply :ok state not-found (if (detail/same-state? state1 state)
                                                           error1
                                                           (error/nested state1 error1)))
                              (reply status1 state1 value1 error1)))))))

;; TODO
;; Maybe two variants: All errors and only when changed state (`catch` and `catch!`)
;; `attempt` could be: `(catch p #(fail-with error))`
;; alternate names: except
#_(defn catch
    "When `p` fails, backtracks and resumes with the parser returned by
   `(f error)`."
    [p f])

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 error/merge))
  ([p1 p2 merge-errors]
   (fn [state reply]
     (detail/call p1 state
                  (fn [status1 state1 value1 error1]
                    (if (detail/ok? status1)
                      (reply status1 state1 value1 error1)
                      (if (nil? error1)
                        (detail/call p2 state reply)
                        (detail/call p2 state
                                     (fn [status2 state2 value2 error2]
                                       (reply status2 state2 value2
                                              (if (detail/same-state? state1 state2)
                                                (merge-errors error1 error2)
                                                error2)))))))))))

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
     (let [merge-err (constantly (error/expected label))]
       (reduce #(alt2 %1 %2 merge-err) p (next ps)))
     pnil)))

;;---------------------------------------------------------
;; Conditional parsing and looking ahead

;; ? Pass `label`?
(defn not-empty
  "Like `p`, but fails when `p` does not change the parser state."
  [p]
  (fn [state reply]
    (detail/call p state (fn [status1 state1 value1 error1]
                           (reply (if (and (detail/ok? status1)
                                           (detail/same-state? state1 state))
                                    :error
                                    status1)
                                  state1 value1 error1)))))

;; fparsec: followedBy, followedByL
;; alternative names: follows, assert-next
(defn followed-by
  ([p]
   (followed-by p nil))
  ([p label]
   (let [error (some-> label error/expected)]
     (fn [state reply]
       (detail/call p state (fn [status1 _ _ _]
                              (if (detail/ok? status1)
                                (reply :ok state nil nil)
                                (reply :error state nil error))))))))

;; fparsec: notFollowedBy, notFollowedByL
;; alternative names: assert-not-next
(defn not-followed-by
  ([p]
   (not-followed-by p nil))
  ([p label]
   (let [error (some-> label error/unexpected)]
     (fn [state reply]
       (detail/call p state (fn [status1 _ _ _]
                              (if (detail/ok? status1)
                                (reply :error state nil error)
                                (reply :ok state nil nil))))))))

;; fparsec: lookAhead
;; names: ?=, ?!, ?<=, ?<!
(defn look-ahead
  [p]
  (fn [state reply]
    (detail/call p state (fn [status1 state1 value1 error1]
                           (if (detail/ok? status1)
                             (reply :ok state value1 nil)
                             (reply :error state nil (error/nested state1 error1)))))))

;;---------------------------------------------------------
;; Customizing error messages

(defn fail [message]
  (let [error (error/message message)]
    (fn [state reply]
      (reply :error state nil error))))

(defn fatal [message]
  (let [error (error/message message)]
    (fn [state reply]
      (reply :fatal state nil error))))

;; fparsec: <?>
(defn as
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (let [error (error/expected label)]
    (fn [state reply]
      (detail/call p state (fn [status1 state1 value1 error1]
                             (reply status1 state1 value1
                                    (if (detail/same-state? state1 state)
                                      error
                                      error1)))))))

;; fparsec: <??>
(defn as! [p label]
  (let [error (error/expected label)]
    (fn [state reply]
      (detail/call p state
                   (fn [status1 state1 value1 error1]
                     (cond
                       (detail/ok? status1)
                       (reply status1 state1 value1
                              (if (detail/same-state? state1 state)
                                error
                                error1))
                       (detail/same-state? state1 state)
                       (reply status1 state1 nil
                              (if (error/message? error1 ::error/nested)
                                (error/nested->compound error1 label)
                                error))
                       :else
                       ;; Backtracked -- reply fatal failure to make sure
                       ;; normal parsing doesn't continue.
                       ;; TODO: Is it worth having a fatal error status just for this use case?
                       (reply :fatal state nil (error/compound label state1 error1))))))))

;;---------------------------------------------------------
;; Sequences / seqexp

(defn cats [ps]
  (detail/reduce-sequence detail/seqexp-rf ps))

(defn cat [& ps]
  (detail/reduce-sequence detail/seqexp-rf ps))

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
  (detail/reduce-repeat `* p detail/seqexp-rf 0))

(defn +
  "One or more."
  [p]
  (detail/reduce-repeat `+ p detail/seqexp-rf 1))

;; fparsec: skipMany
;; or: *skip
(defn skip* [p]
  (detail/reduce-repeat `skip* p detail/ignore 0))

;; fparsec: skipMany1
;; or: +skip
(defn skip+ [p]
  (detail/reduce-repeat `skip+ p detail/ignore 1))

;; fparsec: parray, +skipArray
(defn repeat
  ([p n]
   (detail/reduce-repeat `repeat p detail/seqexp-rf n n))
  ([p min max]
   (detail/reduce-repeat `repeat p detail/seqexp-rf min max)))

(defn min [p n]
  (detail/reduce-repeat `min p detail/seqexp-rf n))

(defn max [p n]
  (detail/reduce-repeat `max p detail/seqexp-rf 0 n))

(defn sep-by* [p sep]
  (detail/reduce-sep `sep-by* p sep detail/vector-rf true false))

(defn sep-by+ [p sep]
  (detail/reduce-sep `sep-by+ p sep detail/vector-rf false false))

(defn skip-sep-by* [p sep]
  (detail/reduce-sep `skip-sep-by* p sep detail/ignore true false))

(defn skip-sep-by+ [p sep]
  (detail/reduce-sep `skip-sep-by+ p sep detail/ignore false false))

(defn sep-end-by* [p sep]
  (detail/reduce-sep `sep-end-by* p sep detail/vector-rf true true))

(defn sep-end-by+ [p sep]
  (detail/reduce-sep `sep-end-by+ p sep detail/vector-rf false true))

(defn skip-sep-end-by* [p sep]
  (detail/reduce-sep `skip-sep-end-by* p sep detail/ignore true true))

(defn skip-sep-end-by+ [p sep]
  (detail/reduce-sep `skip-sep-end-by+ p sep detail/ignore false true))

;; fparsec: manyTill + variants

(defn till* [p endp]
  (detail/reduce-till `till* p endp detail/vector-rf true false))

(defn till+ [p endp]
  (detail/reduce-till `till+ p endp detail/vector-rf false false))

;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Lazy / recursive

;; Note that we can use #'var as well
(defmacro lazy [& body]
  `(detail/pforce (delay ~@body)))

;; fparsec: createParserForwardedToRef
(defn ref
  "Returns a parser that forwards all calls to the parser returned
   by `@ref`.  Useful to construct recursive parsers."
  [ref]
  (fn [state reply]
    (let [p @ref]
      (detail/thunk (p state reply)))))

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
  [state reply]
  (reply :ok state (state/index state) nil))

;; TODO: This assumes an underlying char stream?
(defn pos
  "Returns the current position in the input stream."
  [state reply]
  (reply :ok state (state/pos state) nil))

(defn user-state
  "Returns the current user state."
  [state reply]
  (reply :ok state (state/user-state state) nil))

(defn set-user-state
  "Sets the user state to `u`."
  [u]
  (fn [state reply]
    (reply :ok (state/with-user-state state u) u nil)))

(defn swap-user-state
  "Sets ths user state to `(apply f user-state args)`."
  [f & args]
  (fn [state reply]
    (let [u (apply f (state/user-state state) args)]
      (reply :ok (state/with-user-state state u) u nil))))

(defn match-user-state
  "Succeeds if `pred` returns logical true when called with the current
   user state, otherwise it fails.  Returns the return value of `pred`."
  [pred]
  (fn [state reply]
    (if-let [ret (pred (state/user-state state))]
      (reply :ok state ret nil)
      (reply :error state nil error/no-message))))

;;---------------------------------------------------------
;; Nesting parsers

;; TODO: Track `index`
;; Maybe: cache indexes of newlines seen, so that we can
;; efficiently translate between index and line/col?
(defrecord Token [value pos])

(defn token
  "Behaves like `p`, but wraps its return value in a `Token` record."
  [p]
  (with [pos pos, val p]
    (return (Token. val pos))))

(defn tokenizer
  "Tokenizes the input stream using the parser `p`, skipping over tokens
   matching `skip-p` (e.g. whitespace).  Returns a sequence of `Token`
   records."
  [p skip-p]
  (then (? skip-p) (sep-end-by* (token p) skip-p)))

(defn embed
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
