(ns paco.core
  (:refer-clojure :exclude [* + cat deref map max min not-empty ref repeat sequence])
  (:require [paco.detail.error :as error]
            [paco.detail.parsers :as dp]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.source :as source])
  #?(:cljs (:require-macros [paco.core :refer [fwd]])))

(defn- result-data [source reply]
  {:ok?        (reply/ok? reply)
   :value      (reply/value reply)
   :error      (reply/error reply)
   :index      (source/index source)
   :position   (source/position source)
   :user-state (source/user-state source)})

(defn run
  "Applies the parser `p` to `input` and returns a map representing the
   parser result."
  [p input & {:as opts}]
  (let [source (source/of input opts)
        reply  (p source (reply/mutable-reply))]
    (result-data source reply)))

(defn- parse-exception [source reply]
  (let [{:keys [error position] :as data} (result-data source reply)]
    (ex-info (error/string error position)
             (assoc data :type ::parse-error))))

(defn parse
  "Applies the parser `p` to `input` and returns its result value.  Throws
   an exception when `p` fails."
  [p input & {:as opts}]
  (let [source (source/of input opts)
        reply  (p source (reply/mutable-reply))]
    (if (reply/ok? reply)
      (reply/value reply)
      (throw (parse-exception source reply)))))

;;---------------------------------------------------------
;; Basic parsers

;; fparsec: preturn, >>%
(defn return
  ([x]
   (fn [_source reply]
     (reply/ok reply x)))
  ([p x]
   (fn [source reply]
     (reply/with-value (p source reply) x))))

;; fparsec: pzero, but fails
;; alternate names: null, empty, eps(ilon)
(def pnil
  "This parser always succeeds and returns `nil`."
  (return nil))

(defn end
  "This parser succeeds a the end of the input stream."
  [source reply]
  (if (source/end? source)
    (reply/ok reply nil)
    (reply/fail reply error/expected-end)))

(defn any-token [source reply]
  (if-some [token (source/peek source)]
    (do
      (source/skip! source)
      (reply/ok reply token))
    (reply/fail reply (error/unexpected-token-or-end source))))

(defn skip-any-token [source reply]
  (if (pos? (source/skip! source))
    (reply/ok reply nil)
    (reply/fail reply (error/unexpected-token-or-end source))))

(defn token-return [token value]
  (let [expected (error/expected-input token)]
    (fn [source reply]
      (if (= token (source/peek source))
        (do
          (source/skip! source)
          (reply/ok reply value))
        (reply/fail reply (error/merge expected (error/unexpected-token-or-end source)))))))

(defn token [token]
  (token-return token token))

;;---------------------------------------------------------
;; Chaining and piping

(defn bind [p f]
  (fn [source reply]
    (let [reply (p source reply)]
      (if (reply/ok? reply)
        (let [p2 (f (reply/value reply))]
          (if-some [error (reply/error reply)]
            (let [modcount (source/modcount source)
                  reply    (p2 source reply)]
              (if (= modcount (source/modcount source))
                (reply/with-error reply (error/merge error (reply/error reply)))
                reply))
            (p2 source reply)))
        reply))))

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
  "Returns a parser that applies the provided parsers in sequence and returns
   the return value of calling `f` with the return values."
  ([p f]
   (dp/with-seq [x p] (f x)))
  ([p1 p2 f]
   (dp/with-seq [x p1, y p2] (f x y)))
  ([p1 p2 p3 f]
   (dp/with-seq [x p1, y p2, z p3] (f x y z)))
  ([p1 p2 p3 p4 & more]
   (let [ps (list* p1 p2 p3 p4 (butlast more))
         f  (last more)]
     (dp/sequence (completing conj #(apply f %)) ps))))

(defn tuple*
  "Applies the parsers `ps` in sequence and returns a vector of
   their return values."
  [ps]
  (dp/sequence rfs/vector ps))

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
   (dp/sequence rfs/last (list* p1 p2 p3 p4 more))))

;; fparsec: .>>
(defn then-skip
  "Applies the parsers in sequence and returns the result of the first one."
  ([p] p)
  ([p1 p2]
   (dp/with-seq [x p1, _ p2] x))
  ([p1 p2 p3]
   (dp/with-seq [x p1, _ p2, _ p3] x))
  ([p1 p2 p3 p4 & more]
   (dp/sequence rfs/first (list* p1 p2 p3 p4 more))))

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
    (fn [_source reply]
      (reply/fail reply error))))

;; fparsec: <?>
(defn label
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (let [expected-error (error/expected label)]
    (fn [source reply]
      (let [modcount (source/modcount source)
            reply    (p source reply)]
        (if (= modcount (source/modcount source))
          (reply/with-error reply expected-error)
          reply)))))

;; fparsec: <??>
(defn label-compound
  [p label]
  (let [expected-error (error/expected label)]
    (fn [source reply]
      (source/with-resource [mark (source/mark source)]
        (let [reply (p source reply)]
          (if (reply/ok? reply)
            (if (source/at? source mark)
              (reply/with-error reply expected-error)
              reply)
            (let [error (reply/error reply)]
              (if (source/at? source mark)
                (reply/with-error reply (if (error/message? error ::error/nested)
                                          (error/nested->compound error label)
                                          expected-error))
                (do
                  ;; Backtrack, but keep the source marked as modified, so that
                  ;; normal parsing does not continue.
                  (source/backtrack-modified! source mark)
                  (reply/with-error reply (error/compound label source error)))))))))))

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover, !, atomic, unit
;; TODO: Add an arity (atomic p label) for (atomic (label-compound p label))
(defn atomic
  "Returns a parser that behaves like `p`, except that it backtracks to
   the original parser state when `p` fails with changing the parser
   state."
  [p]
  (fn [source reply]
    (source/with-resource [mark (source/mark source)]
      (let [reply (p source reply)]
        (if (or (reply/ok? reply) (source/at? source mark))
          reply
          (let [error (error/nested source (reply/error reply))]
            (source/backtrack! source mark)
            (reply/fail reply error)))))))

(defn ?atomic
  "Returns a parser that applies `p` and if `p` fails, backtracks to the
   original state and _succeeds_ with value `not-found` (default: `nil`).

   `(?atomic p)` is an optimized implementation of `(? (atomic p))`."
  ([p] (?atomic p nil))
  ([p not-found]
   (fn [source reply]
     (source/with-resource [mark (source/mark source)]
       (let [reply (p source reply)]
         (if (reply/ok? reply)
           reply
           (if (source/at? source mark)
             (reply/ok reply not-found (reply/error reply))
             (let [error (error/nested source (reply/error reply))]
               (source/backtrack! source mark)
               (reply/ok reply not-found error)))))))))

;; TODO: Error handling parsers
;; Maybe two variants: All errors and only when changed state (`catch` and `catch!`)
;; on-error: (f source reply original-state) => detail
;; variant: on-error-apply: backtrack and fall back to another one?
;; alternate names: except
#_(defn catch
    "When `p` fails, backtracks and resumes with the parser returned by
   `(f error)`."
    [p f])

;; Advanced: Full control.  Handler needs to supply a reply.
#_(defn recover
    "Applies the parser `p`.  If `p` fails, calls:

   `(f source mark error reply)`"
    [p f])

;;---------------------------------------------------------
;; Parsing alternatives

;; ? Move to detail.parsers?
(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 true))
  ([p1 p2 merge-errors?]
   (fn [source reply]
     (let [modcount (source/modcount source)
           reply    (p1 source reply)]
       (if (or (reply/ok? reply)
               (not= modcount (source/modcount source)))
         reply
         (let [error (reply/error reply)
               reply (p2 source reply)]
           (if (and merge-errors? (= modcount (source/modcount source)))
             (reply/with-error reply (error/merge error (reply/error reply)))
             reply)))))))

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
  (fn [source reply]
    (let [modcount (source/modcount source)
          reply    (p source reply)]
      (if (= modcount (source/modcount source))
        (reply/with-ok reply false)
        reply))))

;; fparsec: followedBy, followedByL
;; alternative names: peek, follows, assert-next
(defn followed-by
  "Returns a parser that succeeds if it is followed by `p`, e.g. if `p`
   succeeds next.  Does not change the parser state."
  ([p]
   (followed-by p nil))
  ([p label]
   (let [expected-error (some-> label error/expected)]
     (fn [source reply]
       (source/with-resource [mark (source/mark source)]
         (let [reply (p source reply)]
           ;; TODO: Benchmark if we need the conditional
           (when-not (source/at? source mark)
             (source/backtrack! source mark))
           (if (reply/ok? reply)
             ;; Could keep the reply value, like `look-ahead`
             (reply/ok reply nil)
             (reply/fail reply expected-error))))))))

;; fparsec: notFollowedBy, notFollowedByL
;; alternative names: assert-not-next, not, ?! (regex negative lookahead)
(defn not-followed-by
  "Returns a parser that succeeds if the parser `p` fails, and fails otherwise.
   In both cases, it does not change the parser state."
  ([p]
   (not-followed-by p nil))
  ([p label]
   (let [expected-error (some-> label error/unexpected)]
     (fn [source reply]
       (source/with-resource [mark (source/mark source)]
         (let [reply (p source reply)]
           ;; TODO: Benchmark if we need the conditional
           (when-not (source/at? source mark)
             (source/backtrack! source mark))
           (if (reply/ok? reply)
             (reply/fail reply expected-error)
             (reply/ok reply nil))))))))

;; fparsec: lookAhead
;; alternate names: peek; ?= (regex positive look-ahead)
;; TODO: The difference to `followed-by` is subtle. Do we need both?
;; fparsec's followedBy returns `unit` (nil), while `lookAhead` returns the
;; parser's value. followedBy/notFollowedBy are pure assertions/predicates
(defn look-ahead
  [p]
  (fn [source reply]
    (source/with-resource [mark (source/mark source)]
      (let [reply (p source reply)]
        (if (reply/ok? reply)
          (do
            ;; TODO: Benchmark if we need the conditional
            (when-not (source/at? source mark)
              (source/backtrack! source mark))
            ;; Discard error messages
            (reply/with-error reply nil))
          (if (source/at? source mark)
            reply
            (let [error (error/nested source (reply/error reply))]
              (source/backtrack! source mark)
              (reply/fail reply error))))))))

;;---------------------------------------------------------
;; Sequences / seqexp

;;? Add a fn to treat a sequex as one unit to prevent flattening?

(defn cat* [ps]
  (dp/sequence rfs/seqex ps))

(defn cat [& ps]
  (dp/sequence rfs/seqex ps))

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

(defn *sep-by
  ([p psep] (*sep-by p psep nil))
  ([p psep end?]
   (dp/sep-by `*sep-by p psep rfs/vector true end?)))

(defn +sep-by
  ([p psep] (+sep-by p psep nil))
  ([p psep end?]
   (dp/sep-by `+sep-by p psep rfs/vector false end?)))

(defn *skip-sep-by
  ([p psep] (*skip-sep-by p psep nil))
  ([p psep end?]
   (dp/sep-by `*skip-sep-by p psep rfs/ignore true end?)))

(defn +skip-sep-by
  ([p psep] (+skip-sep-by p psep nil))
  ([p psep end?]
   (dp/sep-by `+skip-sep-by p psep rfs/ignore false end?)))

;; fparsec: manyTill
(defn *until
  ([p pend] (*until p pend nil))
  ([p pend include-end?]
   (dp/until `*until p pend rfs/vector true include-end?)))

(defn +until
  ([p pend] (+until p pend nil))
  ([p pend include-end?]
   (dp/until `+until p pend rfs/vector false include-end?)))

(defn *skip-until [p pend]
  (dp/until `*skip-until p pend rfs/ignore true false))

(defn +skip-until [p pend]
  (dp/until `+skip-until p pend rfs/ignore false false))

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
  `(fn [source# reply#]
     (let [p# ~expr]
       (p# source# reply#))))

(defn pforce
  "Returns a parser that forwards to `(force d)`."
  [d]
  (fwd (clojure.core/force d)))

;; alternate name: pdelay, delay, defer
(defmacro lazy [& body]
  `(pforce (clojure.core/delay ~@body)))

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
  [source reply]
  (reply/ok reply (source/index source)))

;; TODO: This assumes an underlying char stream? => move to `paco.char`?
(defn pos
  "Returns the current position in the input stream."
  [source reply]
  (reply/ok reply (source/position source)))

(defn user-state
  "Returns the current user state."
  [source reply]
  (reply/ok reply (source/user-state source)))

(defn user-state-satisfy
  "Succeeds if `pred` returns logical true when called with the current
   user source, otherwise it fails.  Returns the return value of `pred`."
  [pred]
  (fn [source reply]
    (if-let [ret (pred (source/user-state source))]
      (reply/ok reply ret)
      (reply/fail reply error/no-message))))

(defn set-user-state
  "Sets the user source to `u`."
  [u]
  (fn [source reply]
    (source/with-user-state! source u)
    (reply/ok reply u)))

(defn swap-user-state
  "Sets ths user source to `(apply f user-state args)`."
  [f & args]
  (fn [source reply]
    (let [u (apply f (source/user-state source) args)]
      (source/with-user-state! source u)
      (reply/ok reply u))))
