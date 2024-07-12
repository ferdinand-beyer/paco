(ns paco.core
  (:refer-clojure :exclude [* + cat cond deref force map max min not not-empty peek ref repeat sequence])
  (:require [paco.detail.advanced :as advanced]
            [paco.detail.error :as error]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.source :as source])
  #?(:cljs (:require-macros [paco.core :refer [fwd let-return]])))

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

(defn return
  "With one arg, returns a parser that consumes nothing and returns `x`.

   With two args, returns a parser that behaves like `p`, but returns `x`
   when `p` succeeds.

   Similar to:
   - fparsec: `preturn`, `>>%`"
  ([x]
   (fn [_source reply]
     (reply/ok reply x)))
  ([p x]
   (fn [source reply]
     (reply/with-value (p source reply) x))))

(def eps
  "The eps(ilon) parser accepts the empty word (i.e. it always succeeds) and
   returns `nil`.

   Similar to:
   - fparsec: `pzero` (but succeeds)"
  (return nil))

(defn end
  "The `end` parser only succeeds a the end of the input stream and
   returns `nil`.

   Similar to:
   - parsec, fparsec: `eof`"
  [source reply]
  (if (source/end? source)
    (reply/ok reply nil)
    (reply/fail reply error/expected-end)))

(defn any-token
  "This parser accepts and returns any token from the input stream."
  [source reply]
  (if-some [token (source/peek source)]
    (do
      (source/skip! source)
      (reply/ok reply token))
    (reply/fail reply (error/unexpected-token-or-end source))))

(defn skip-any-token
  "This parser skips the next token from the input stream and returns `nil`."
  [source reply]
  (if (pos? (source/skip! source))
    (reply/ok reply nil)
    (reply/fail reply (error/unexpected-token-or-end source))))

(defn token-return
  "Returns a parser that expects `token` to come next in the input stream
   and returns `value`."
  [token value]
  (let [expected (error/expected-input token)]
    (fn [source reply]
      (if (= token (source/peek source))
        (do
          (source/skip! source)
          (reply/ok reply value))
        (reply/fail reply (error/merge expected (error/unexpected-token-or-end source)))))))

(defn token
  "Returns a parser that accepts and returns `token`."
  [token]
  (token-return token token))

;;---------------------------------------------------------
;; Chaining and piping

(defn bind
  "Returns a parser that first applies the parser `p` to the input, then
   calls the function `f` with the result value of `p` and finally applies
   the parser returned by `f` to the input.

   Similar to:
   - parsec, fparsec: `>>=`"
  [p f]
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

(defn- emit-let-parser [bindings body]
  (let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-let-parser (drop 2 bindings) body))))))

(defmacro let-parser
  "Similar to `let`, but for parsers.  The expressions in `bindings` must
   evaluate to parsers.  The returned parser sequentially applies these parsers
   to the input, and binds their result values to their binding forms.  Every
   expression can use the preceding bindings.

   Finally, it evaluates `body` with all bindings, which must yield a parser that
   is then applied to the input.

   Expands to a chain of `bind` calls.

   Use this to create parsers dynamically depending on the return value of a
   previously succeeded parser.  If you don't need this functionality, consider
   using `let-return` instead."
  {:clj-kondo/lint-as 'clojure.core/let
   :style/indent 1}
  [bindings & body]
  {:pre [(vector? bindings)
         (even? (count bindings))
         (some? body)]}
  (emit-let-parser bindings body))

(defn- emit-let-return [bindings body]
  {:pre [(vector? bindings)
         (seq bindings)
         (even? (count bindings))]}
  (let [source (gensym "source__")
        reply  (gensym "reply__")
        emit   (fn emit [[binding-form parser & more-bindings] modcount-before error-before]
                 `(let [~reply (~parser ~source ~reply)]
                    (if (reply/ok? ~reply)
                      (let [~binding-form (reply/value ~reply)]
                        ~(if (seq more-bindings)
                           ;; recur to next parsers
                           (let [modcount (gensym "modcount__")
                                 error    (gensym "error__")]
                             `(let [~modcount (source/modcount ~source)
                                    ~error    (reply/error ~reply)]
                                ~(emit more-bindings
                                       modcount
                                       (if error-before
                                         `(if (= ~modcount-before ~modcount)
                                            (error/merge ~error-before ~error)
                                            ~error)
                                         error))))
                           ;; this was the last parser
                           (if error-before
                             `(let [value# (do ~@body)]
                                (if (= ~modcount-before (source/modcount ~source))
                                  (reply/ok ~reply value# (error/merge ~error-before (reply/error ~reply)))
                                  (reply/with-value ~reply value#)))
                             `(reply/with-value ~reply (do ~@body)))))
                      ;; parser failed
                      ~(if error-before
                         `(let [error# (reply/error ~reply)]
                            (if (= ~modcount-before (source/modcount ~source))
                              (reply/with-error ~reply (error/merge ~error-before error#))
                              ~reply))
                         reply))))]
    (list `fn [source reply] (emit bindings nil nil))))

(defmacro let-return
  "Like `let-parser`, but evaluates `body` as the resulting parser's result
   value, instead of expecting a final parser to apply.

   The parser `(let-return [x p] (f x))` is an optimised version of
   `(let-parser [x p] (return (f x)))`."
  {:clj-kondo/lint-as 'clojure.core/let
   :style/indent 1}
  [bindings & body]
  (emit-let-return bindings body))

(defn map
  "Returns a parser that applies the parser `p` and returns the return value
   of calling `f` with the result value of `p`.

   When multiple parsers are given, applies them in sequence and calls `f` with
   all result values.  The arity of `f` must match the number of supplied
   parsers.

   Similar to:
   - parsec : `fmap`
   - fparsec: `|>>`, `pipe2`, `pipe3`, `pipe4`, `pipe5`"
  ([p f]
   (let-return [x p] (f x)))
  ([p1 p2 f]
   (let-return [x p1, y p2] (f x y)))
  ([p1 p2 p3 f]
   (let-return [x p1, y p2, z p3] (f x y z)))
  ([p1 p2 p3 p4 & more]
   (let [ps (list* p1 p2 p3 p4 (butlast more))
         f  (last more)]
     (advanced/sequence (completing conj #(apply f %)) ps))))

(defn tuples
  "Returns a parser that applies the parsers `ps` in sequence and returns a
   vector of their return values."
  [ps]
  (advanced/sequence rfs/vector ps))

(defn tuple
  "Returns a parser that applies the parsers in sequence and returns a
   vector of their return values.

   Similar to:
   - parsec: `<*>`
   - fparsec: `.>>.`, `tuple2`, `tuple3`, `tuple4`, `tuple5`"
  ([p]
   (map p vector))
  ([p1 p2]
   (map p1 p2 vector))
  ([p1 p2 p3]
   (map p1 p2 p3 vector))
  ([p1 p2 p3 p4 & more]
   (tuples (list* p1 p2 p3 p4 more))))

(defn then
  "Returns a parser that applies the parsers `ps` in sequence and returns the
   result of the last one.

   Similar to:
   - parsec: `>>`
   - fparsec: `>>.`"
  ([p] p)
  ([p1 p2]
   (let-return [_ p1, x p2] x))
  ([p1 p2 p3]
   (let-return [_ p1, _ p2, x p3] x))
  ([p1 p2 p3 p4 & more]
   (advanced/sequence rfs/last (list* p1 p2 p3 p4 more))))

(defn then-skip
  "Returns a parser that applies the parsers `ps` in sequence and
   returns the result of the first one.

   Similar to:
   - fparsec: `.>>`"
  ([p] p)
  ([p1 p2]
   (let-return [x p1, _ p2] x))
  ([p1 p2 p3]
   (let-return [x p1, _ p2, _ p3] x))
  ([p1 p2 p3 p4 & more]
   (advanced/sequence rfs/first (list* p1 p2 p3 p4 more))))

(defn between
  "Returns a parser that applies the parsers `popen`, `p` and `pclose` in sequence
   and returns the result of `p`.

   When called with two args, uses `pdelim` for both `popen` and `pclose`."
  ([p pdelim]
   (between p pdelim pdelim))
  ([p popen pclose]
   (let-return [_ popen, x p, _ pclose] x)))

;;---------------------------------------------------------
;; Customizing and recovering from errors

(defn fail
  "Returns a parser that always fails with given error message."
  [message]
  (let [error (error/message message)]
    (fn [_source reply]
      (reply/fail reply error))))

(defn label
  "Returns a parser that behaves like `p`, but replaces errors with
   `(expected label)` when `p` not change the parser state.

   Similar to:
   - fparsec: `<?>"
  [p label]
  (let [expected-error (error/expected label)]
    (fn [source reply]
      (let [modcount (source/modcount source)
            reply    (p source reply)]
        (if (= modcount (source/modcount source))
          (reply/with-error reply expected-error)
          reply)))))

(defn label-compound
  "Returns a parser that behaves like `(label p)`, but generates
   a \"compund error\" message with both the given string label and the
   error messages generated by p.

   Similar to:
   - fparsec: `<??>"
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
;; TODO: Add an arity (atomic p label) for (atomic (label-compound p label))
(defn atomic
  "Returns a parser that behaves like `p`, but is atomic: When `p` fails
   after changing the parser state, backtracks to undo the change.

   Similar to:
   - parsec: `try`
   - fparsec: `attempt`"
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
  "Returns a parser that applies the _optional_ parser `p`. If `p` fails,
   backtracks to the original state and _succeeds_ with value `not-found`
   (default: `nil`).

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

(defn alt
  "Returns a parser that applies the parsers until the first one succeeds
   or fails after changing the parser state.

   Similar to:
   - parsec, fparsec: `<|>`, `choice`"
  ([p1] p1)
  ([p1 p2]
   (alt2 p1 p2))
  ([p1 p2 p3]
   (-> p1 (alt2 p2) (alt2 p3)))
  ([p1 p2 p3 & more]
   (reduce alt2 p1 (list* p2 p3 more))))

(defn alts
  "Like `alt`, but takes a sequence of parsers.

   Providing a `label` is slightly faster, because the parser doesn't
   have to aggregate error messages.

   - parsec, fparsec: `choice`, `choiceL`"
  ([ps]
   (if-let [p (first ps)]
     (reduce alt2 p (next ps))
     eps))
  ([ps label']
   (if-let [p (first ps)]
     (label (reduce #(alt2 %1 %2 false) p (next ps)) label')
     eps)))

;;---------------------------------------------------------
;; Conditional parsing and looking ahead

;; ? Pass `label`?
(defn not-empty
  "Returns a parser that behaves like `p`, but fails when `p` does not
   change the parser state."
  [p]
  (fn [source reply]
    (let [modcount (source/modcount source)
          reply    (p source reply)]
      (if (= modcount (source/modcount source))
        (reply/with-ok reply false)
        reply))))

;; fparsec: followedBy, followedByL
;; alternative names: ?= (regex positive look-ahead)
(defn followed-by
  "EXPERIMENTAL: The difference between `followed-by` and `peek` (`lookAhead`)
   is subtle: fparsec's `followedBy` is an assertion and returns nothing, while
   `lookAhead` returns the wrapped parser's result.  Do we need both?

   Returns a parser that succeeds if `p` succeeds, but does not change
   the parser state.  Returns `nil`.

   Similar to:
   - fparsec: `followedBy`, `followedByL`"
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

;; alternative names: ?! (regex negative lookahead)
(defn not
  "Returns a parser that succeeds if the parser `p` fails, and fails otherwise.
   In both cases, it does not change the parser state.  Returns `nil`.

   Similar to:
   - parsec: `notFollowedBy`
   - fparsec: `notFollowedBy`, `notFollowedByL`"
  ([p]
   (not p nil))
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

(defn peek
  "Returns a parser that behaves like `p`, but always restores the parser
   state.

   - parsec, fparsec: `lookAhead`"
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

(defn cond-bind
  "Returns a parser that first applies `p`.  Then, if `p` succeeds,
   applies the parser returned by calling `then-fn` with the result of `p`.
   Otherwise, if `p` fails without changing the parser state, applies
   `pelse`.  If `p` fails after changing the parser state, so does the
   `cond-bind` parser."
  ([p then-fn] (bind p then-fn))
  ([p then-fn pelse]
   (fn [source reply]
     (let [modcount (source/modcount source)
           reply    (p source reply)
           error    (reply/error reply)]
       (if (reply/ok? reply)
         (let [modcount (source/modcount source)
               pthen    (then-fn (reply/value reply))
               reply    (pthen source reply)]
           (cond-> reply
             (= modcount (source/modcount source))
             (reply/with-error (error/merge error (reply/error reply)))))
         (if (= modcount (source/modcount source))
           (-> (pelse source reply)
               (reply/with-error (error/merge error (reply/error reply))))
           ;; p failed after changing the parser state
           reply))))))

(defn cond
  "Returns a parser that first applies `p`.  Then, if `p` succeeds,
   applies `pthen`.  Otherwise, if `p` fails without changing the parser
   state, applies `pelse`.  If `p` fails after changing the parser state,
   so does the `cond` parser."
  ([p pthen] (then p pthen))
  ([p pthen pelse] (cond-bind p (constantly pthen) pelse)))

;;---------------------------------------------------------
;; Sequences / seqexp

(defn cat
  "Returns a parser that applies the given parsers in sequence,
   and returns a collection of their result values (concatenation).

   This is a 'sequence expression' parser: the result of nested sequence
   expressions are flattened in this parser's return value.
   `(cat p1 (cat p2 p3))` is logically equivalent to `(cat p1 p2 p3)`."
  [& ps]
  (advanced/sequence rfs/seqex ps))

(defn cats
  "Like `cat`, but takes a sequence of parsers."
  [ps]
  (advanced/sequence rfs/seqex ps))

(defn group
  "Like `cat`, but returns a collection that will not be flattened into
   surrounding sequence expressions."
  ([p] (map p rfs/unseqex))
  ([p1 p2 & ps]
   (advanced/sequence rfs/seqex-group (list* p1 p2 ps))))

(defn ?
  "Returns a parser that parses an optional occurrence of `p`.

   When `p` fails without changing parser state, `(? p)` succeeds with `nil`,
   and `(? p not-found)` with `not-found`.

   When `p` fails after changing parser state, so will `(? p)`.

   This is a 'sequence expression' parser, see `cat`.

   Similar to:
   - parsec, fparsec: `option`, `optional`"
  ([p]
   (alt2 p eps))
  ([p not-found]
   (alt2 p (return not-found))))

(defn *
  "Returns a parser that parses zero or more occurrances of `p`, and returns
   a collection of the return values.

   This is a 'sequence expression' parser, see `cat`.

   Similar to:
   - parsec, fparsec: `many`"
  [p]
  (advanced/repeat-many `* p rfs/seqex))

(defn +
  "Returns a parser that parses one or more occurrances of `p`, and returns
   a collection of the return values.

   This is a 'sequence expression' parser, see `cat`.

   Similar to:
   - parsec, fparsec: `many1`"
  [p]
  (advanced/repeat-min `+ p rfs/seqex 1))

(defn *skip
  "Like `*`, but discards `p`'s results and returns `nil`.

   Simillar to:
   - fparsec: `skipMany`"
  [p]
  (advanced/repeat-many `*skip p rfs/ignore))

(defn +skip
  "Like `+`, but discards `p`'s results and returns `nil`.

   Simillar to:
   - fparsec: `skipMany1`"
  [p]
  (advanced/repeat-min `+skip p rfs/ignore 1))

(defn repeat
  "When called with two args, returns a parser that applies `p` `n` times.

   When called with three args, returns a parser that applies `p` at least
   `min` times and at most `max` times (until it fails).

   This is a 'sequence expression' parser, see `cat`.

   Similar to:
   - fparsec: `parray`"
  ([p n]
   (advanced/repeat-times `repeat p rfs/seqex n))
  ([p min max]
   (advanced/repeat-min-max `repeat p rfs/seqex min max)))

;; fparsec: skipArray

(defn min
  "Returns a parser that applies `p` `n` or more times, and returns a
   collection of the result values.

   This is a 'sequence expression' parser, see `cat`."
  [p n]
  (advanced/repeat-min `min p rfs/seqex n))

(defn max
  "Returns a parser that applies `p` at most `n` times, and returns a
   collection of the result values.

   This is a 'sequence expression' parser, see `cat`."
  [p n]
  (advanced/repeat-max `max p rfs/seqex n))

(defn *sep-by
  "Returns a parser that parses `p` zero or more times, separated by
   the parser `psep`.  Returns a collection of the return values of `p`.
   The return value of `psep` is discarded.

   When `sep-may-end?` is true, allows the sequence to end with `psep`.
   Otherwise, expects `p` to succeed after every `psep`.

   This is a 'sequence expression' parser, see `cat`.

   Similar to:
   - parsec, fparsec: `sepBy`, `sepEndBy`"
  ([p psep] (*sep-by p psep nil))
  ([p psep sep-may-end?]
   (advanced/sep-by `*sep-by p psep rfs/seqex true sep-may-end?)))

(defn +sep-by
  "Like `*sep-by`, but requires `p` to suceed at least once.

   Similar to:
   - parsec, fparsec: `sepBy1`, `sepEndBy1`"
  ([p psep] (+sep-by p psep nil))
  ([p psep sep-may-end?]
   (advanced/sep-by `+sep-by p psep rfs/seqex false sep-may-end?)))

(defn *skip-sep-by
  "Like `*sep-by`, but discards all results and returns `nil`.

   Similar to:
   - fparsec: `skipSepBy`, `skipSepEndBy`"
  ([p psep] (*skip-sep-by p psep nil))
  ([p psep end?]
   (advanced/sep-by `*skip-sep-by p psep rfs/ignore true end?)))

(defn +skip-sep-by
  "Like `+sep-by`, but discards all results and returns `nil`.

   Similar to:
   - fparsec: `skipSepBy1`, `skipSepEndBy1`"
  ([p psep] (+skip-sep-by p psep nil))
  ([p psep end?]
   (advanced/sep-by `+skip-sep-by p psep rfs/ignore false end?)))

(defn *until
  "Returns a parser that repeatedly applies the parser `p` for as long
   as `pend` fails (without changing the parser state).  It returns a collection
   of the results returned by `p`.

   When `include-end?` is true, also includes the result of `pend` in the
   returned collection.

   Similar to:
   - parsec, fparsec: `manyTill`"
  ([p pend] (*until p pend nil))
  ([p pend include-end?]
   (advanced/until `*until p pend rfs/seqex true include-end?)))

(defn +until
  "Like `*until`, but requires `p` to succeed at least once.

   Similar to:
   - fparsec: `many1Till`"
  ([p pend] (+until p pend nil))
  ([p pend include-end?]
   (advanced/until `+until p pend rfs/seqex false include-end?)))

(defn *skip-until
  "Like `*until`, but discards all results and returns `nil`.

   Similar to:
   - fparsec: `skipManyTill`"
  [p pend]
  (advanced/until `*skip-until p pend rfs/ignore true false))

(defn +skip-until
  "Like `+until`, but discards all results and returns `nil`.

   Similar to:
   - fparsec: `skipMany1Till`"
  [p pend]
  (advanced/until `+skip-until p pend rfs/ignore false false))

;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Forwarding / lazy / recursive parsers

(defmacro fwd
  "Returns a parser that, when applied, evaluates `body` and forwards to the
   resulting parser.

   Useful for recursive parsers, to use a parser that has been declared but not
   yet defined, usually in a var:

   ```clojure
   (def expr (alt digit (between (fwd expr) (char \\() (char \\)))))
   ```

   **Warning**: Left-recursive parsers are not supported and will lead to
   infinite loops / stack overflows."
  [& body]
  `(fn [source# reply#]
     (let [p# (do ~@body)]
       (p# source# reply#))))

(defn force
  "Returns a parser that realizes and applies a delayed parser on demand."
  [d]
  (fwd (clojure.core/force d)))

(defmacro lazy
  "Returns a parser that evaluates `body` the first time it is needed,
   and caches the resulting parser."
  [& body]
  `(force (delay ~@body)))

(defn deref
  "Returns a parser that de-references `ref` on demand, allowing for
   recursive or mutable parsers.

   Similar to:
   - fparsec: `createParserForwardedToRef`"
  [ref]
  (fwd @ref))

(defn rec
  "Creates a recursive parser.  Calls `f` with one arg, a parser to recur,
   and returns the parser returned by `f`.

   It is assumed that `f` uses `alt` or similar to eventually stop the
   recursion.  For example:

   ```clojure
   (def parens (rec #(? (between % (char \\() (char \\))))))
   ```"
  [f]
  (let [vol (volatile! eps)]
    (vreset! vol (f (deref vol)))))

;;---------------------------------------------------------
;; Handling state

(defn index
  "This parser returns the index of the next token in the input stream."
  [source reply]
  (reply/ok reply (source/index source)))

(defn user-state
  "This parser returns the current user state."
  [source reply]
  (reply/ok reply (source/user-state source)))

(defn user-state-satisfy
  "Returns a parser that succeeds if `pred` returns logical
   true when called with the current user state, otherwise it fails.
   Returns the return value of `pred`."
  [pred]
  (fn [source reply]
    (if-let [ret (pred (source/user-state source))]
      (reply/ok reply ret)
      (reply/fail reply error/no-message))))

(defn set-user-state
  "Returns a parser that unconditionally sets the user state to `x`
   and returns swapped-in user state."
  [u]
  (fn [source reply]
    (source/reset-user-state! source u)
    (reply/ok reply u)))

(defn swap-user-state
  "Returns a parser that sets the user state to `(apply f user-state args)`,
   and returns the swapped-in user state."
  [f & args]
  (fn [source reply]
    (let [u (apply f (source/user-state source) args)]
      (source/reset-user-state! source u)
      (reply/ok reply u))))
