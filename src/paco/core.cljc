(ns paco.core
  (:refer-clojure :exclude [* + cat max min ref repeat sequence])
  (:require [clojure.core :as core]
            [paco.error :as error]
            [paco.reply :as reply]
            [paco.state :as state])
  #?(:cljs (:require-macros [paco.core :refer [pipe-parser]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- ok-value [_ value _]
  {:pre [(not (fn? value))]}
  value)

(defn- ok-result [state value error]
  {:status   :ok
   :value    value
   :position (state/pos state)
   :user     (state/user-state state)
   :messages (error/sort-messages error)})

(defn- fail-result [state error]
  {:status   :error
   :position (state/pos state)
   :user     (state/user-state state)
   :messages (error/sort-messages error)})

(defn- parser-exception [{:keys [position messages] :as result}]
  (ex-info (error/string messages position)
           (assoc result :type ::parse-error)))

(defn- fail-exception [state error]
  (throw (parser-exception (fail-result state error))))

(defn- run-parser [p input opts ok fail]
  (trampoline #(p (state/of input opts) (reply/context ok fail))))

(defn run [p input & {:as opts}]
  (run-parser p input opts ok-result fail-result))

(defn parse [p input & {:as opts}]
  (run-parser p input opts ok-value fail-exception))

;;---------------------------------------------------------

;; fparsec: preturn
;; fparsec also has >>%: parse p, but return x
(defn return
  ([x]
   (fn [state ctx]
     (reply/ok ctx state x)))
  ([p x]
   (fn [state ctx]
     (reply/continue p state ctx
       :ok  (fn [s _ e] (reply/ok ctx s x e))
       :ok! (fn [s _ e] (reply/ok! ctx s x e))))))

;; fparsec: pzero, but fails
(defn pnil
  "This parser always succeeds and returns `nil`."
  [state ctx]
  (reply/ok ctx state nil))

;;---------------------------------------------------------
;; Chaining and piping

(defn bind [p f]
  (fn [state ctx]
    (reply/continue p state ctx
      :ok (fn [s1 v1 e1]
            (let [p2 (f v1)]
              (reply/continue p2 s1 ctx
                :ok   (reply/fwd-ok ctx e1)
                :fail (reply/fwd-fail ctx e1))))
      :ok! (fn [s1 v1 e1]
             (let [p2 (f v1)]
               (reply/continue p2 s1 ctx
                 :ok   (reply/fwd-ok! ctx e1)
                 :fail (reply/fwd-fail! ctx e1)))))))

(defn- emit-let [bindings body]
  (core/let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      ;; TODO Emit "do" body?
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-let (drop 2 bindings) body))))))

;; TODO: Rename to `let`
;; alternative names: let->>, >>let, let-seq, plet
(defmacro with
  {:clj-kondo/lint-as 'clojure.core/let
   :style/indent 1}
  [bindings & body]
  {:pre [(vector? bindings)
         (even? (count bindings))
         (some? body)]}
  (emit-let bindings body))

(defn- vec-rf
  "Reducing function that collects input in a vector.
   Like `conj!`, but completes with a persistent collection."
  ([] (transient []))
  ([coll] (persistent! coll))
  ([coll x] (conj! coll x)))

(def ^:private ignore
  "Ignores all args and returns `nil`.  Useful for skip parsers."
  (constantly nil))

;; ? Have a 2-arity ok without messages?
;; saves us from merging messages
;; maybe we can find patterns how we wrap "ok"?
;; and create macros to help us with repetition?

;; Move parsers like these to an `advanced` namespace
;; to make them available to experts?
;; TODO: Also provide a version taking a `xform`?
;; ? Support reduced?
;; alternate names: >>reduce, preduce, reduce-seq
(defn- reduce-sequence
  "Creates a parser that runs `ps` in sequence, reducing their return
   values with the reducing function `f`."
  [f ps]
  (letfn [(run-fn [p run-next]
            (fn [result error state ctx]
              (reply/continue p state ctx
                :ok   (fn [s1 v1 e1]
                        (run-next (f result v1) (error/merge error e1) s1 ctx))
                :ok!  (fn [s1 v1 e1]
                        (run-next (f result v1) e1 s1 (reply/with ctx, :ok :ok!, :fail :fail!)))
                :fail (fn fail [s1 e1]
                        (reply/fail ctx s1 (error/merge error e1))))))
          (complete [result error state ctx]
            (reply/ok ctx state (f result) error))
          (compile [ps]
            (if-let [p (first ps)]
              (run-fn p (compile (next ps)))
              complete))]
    (let [run (compile ps)]
      (fn [state ctx]
        (run (f) nil state ctx)))))

(defn- emit-pipe-body
  [f ps prev-state ctx prev-values prev-error]
  (let [depth      (inc (count prev-values))
        make-sym   (fn [prefix] (symbol (str prefix depth)))
        state      (make-sym "state")
        value      (make-sym "value")
        error      (make-sym "error")
        values     (conj prev-values value)
        merged-err (cond->> error
                     prev-error (list `error/merge prev-error))
        next-ps    (next ps)]
    (list* `reply/continue (first ps) prev-state ctx
           :ok (list `fn (make-sym "ok") [state value error]
                     (if next-ps
                       (emit-pipe-body f next-ps state ctx values merged-err)
                       (list `reply/ok ctx state (cons f values) merged-err)))
           :ok! (list `fn (make-sym "ok!") [state value error]
                      (if next-ps
                        (emit-pipe-body f next-ps state
                                        `(reply/with ~ctx, :ok :ok!, :fail :fail!)
                                        values error)
                        (list `reply/ok! ctx state (cons f values) error)))
           (when prev-error
             (list :fail (list `reply/fwd-fail ctx prev-error))))))

(defn- emit-pipe-parser [ps f]
  (list `fn (symbol (str "pipe" (count ps))) '[state ctx]
        (emit-pipe-body f ps 'state 'ctx [] nil)))

(defmacro ^:private pipe-parser [& ps+f]
  (emit-pipe-parser (butlast ps+f) (last ps+f)))

(comment
  (emit-pipe-parser '[p1] 'f)
  (emit-pipe-parser '[p1 p2] 'f)
  (emit-pipe-parser '[p1 p2 p3] 'f)
  ;;
  )

;; fparsec: |>>, pipe2, pipe3, pipe4, pipe5
(defn pipe
  ([p f]
   (pipe-parser p f))
  ([p1 p2 f]
   (pipe-parser p1 p2 f))
  ([p1 p2 p3 f]
   (pipe-parser p1 p2 p3 f))
  ([p1 p2 p3 p4 & more]
   (let [ps (list* p1 p2 p3 p4 (butlast more))
         f  (last more)]
     (reduce-sequence (completing conj #(apply f %)) ps))))

;; alternative names: g, group', groups, pseq
(defn sequence
  "The parser `(series ps)` applies the parsers `ps` in sequence."
  [ps]
  (reduce-sequence vec-rf ps))

;; fparsec: .>>., tuple2, tuple3, tuple4, tuple5
(defn group
  ([p] p)
  ([p1 p2]
   (pipe p1 p2 list))
  ([p1 p2 p3]
   (pipe p1 p2 p3 list))
  ([p1 p2 p3 p4 & more]
   (sequence (list* p1 p2 p3 p4 more))))

;; fparsec: >>.
;; parsesso: after
;; Would be cool to call this `do`, but this fails for (do)
;; since it is a special form
;; alternative names: then, pdo
(defn >> [& ps]
  (reduce-sequence (fn
                     ([] nil)
                     ([result] result)
                     ([_ input] input))
                   ps))

;; fparsec: .>> - return the first result
;; fparsetc: .>>.: like (cat p1 p2)

(defn between
  ([p psurround]
   (between p psurround psurround))
  ([p popen pclose]
   (pipe popen p pclose (fn [_ x _] x))))

;;---------------------------------------------------------
;; Parsing alternatives and recovering from errors

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover, !
(defn attempt [p]
  (fn [state ctx]
    (reply/continue p state ctx
      :fail! (reply/fail-backtrack ctx state))))

(defn ??
  "Applies `p`.  If `p` fails, `??` will backtrack to the original state
   and succeed with `not-found` or `nil`.

   `(?? p)` is an optimized implementation of `(? (attempt p))`."
  ([p] (?? p nil))
  ([p not-found]
   (fn [state ctx]
     (reply/continue p state ctx
       :fail  (fn fail [state error]
                (reply/ok ctx state not-found error))
       :fail! (reply/ok-backtrack ctx state not-found)))))

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 error/merge))
  ([p1 p2 merge-err]
   (fn [state ctx]
     (reply/continue p1 state ctx
       :fail (fn fail [s1 e1]
               (reply/continue p2 s1 ctx
                 :ok   (fn ok [s2 v2 e2]
                         (reply/ok ctx s2 v2 (merge-err e1 e2)))
                 :fail (fn fail [s2 e2]
                         (reply/fail ctx s2 (merge-err e1 e2)))))))))

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
     (core/let [merge-err (constantly (error/expected label))]
       (reduce #(alt2 %1 %2 merge-err) p (next ps)))
     pnil)))

;;---------------------------------------------------------
;; Conditional parsing and looking ahead

;; fparsec: notEmpty -- require p to change parser state
;;   e.g. to make `*` into `+`
;; fparsec: followedBy, followedByL, notFollowedBy, notFollowedByL
;; fparsec: lookAhead

;;---------------------------------------------------------
;; Customizing error messages

(defn fail [message]
  (core/let [error (error/message message)]
    (fn [state ctx]
      (reply/fail ctx state error))))

;; fparsec: <?>
(defn as
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (core/let [error (error/expected label)]
    (fn [state ctx]
      (reply/continue p state ctx
        :ok   (fn [s1 v1 _]
                (reply/ok ctx s1 v1 error))
        :fail (fn [s1 _]
                (reply/fail ctx s1 error))))))

;; fparsec: <??>
(defn as! [p label]
  (core/let [error (error/expected label)]
    (fn [state ctx]
      (reply/continue p state ctx
        :ok   (fn [s1 v1 _]
                (reply/ok ctx s1 v1 error))
        :fail (fn [s1 e1]
                (if (error/message? e1 ::error/nested)
                  (reply/fail ctx s1 (assoc e1
                                            :type ::error/compound
                                            :label label))
                  (reply/fail ctx s1 error)))
        :fail! (fn [s1 e1]
                 (reply/fail! ctx state (error/compound label s1 e1)))))))

;;---------------------------------------------------------
;; Sequences / seqexp

(defn- cat-rf
  ([] (transient []))
  ([xs] (persistent! xs))
  ([xs x]
   (cond
     (nil? x) xs
     (sequential? x) (reduce conj! xs x)
     :else (conj! xs x))))

;; like group, but flattens
(defn cat [& ps]
  (reduce-sequence cat-rf ps))

;; alternative names: opt, maybe
(defn ?
  "Optional: zero or one."
  ([p]
   (alt2 p pnil))
  ([p not-found]
   (alt2 p (return not-found))))

(defn- state-unchanged-exception [sym p]
  (ex-info (str "Parser supplied to '" sym "' succeeded without changing the parser state")
           {:type ::state-unchanged
            :parser sym
            :arg p}))

(defn- reduce-many
  "Repeatedly applies `p` until `p` fails.  Reduces the results
   produced by `p` using the reducing function `rf`.

   Tries to guard against an infinite loop by throwing an exception
   if `p` succeeds without changing the parser state, reporting a
   `state-unchanged-exception` with the parser name `sym`.

   When `p` fails on the first call (without changing the state),
   i.e. when `p` succeeded zero times, calls
   `(zero ctx state error)`."
  [sym zero f p]
  (letfn [(ok-throw [_ _ _]
            (throw (state-unchanged-exception sym p)))
          (make-ok! [ctx result]
            (fn [state value error]
              (core/let [result (f result value)]
                (reply/continue p state ctx
                  :ok   ok-throw
                  :ok!  (make-ok! ctx result)
                  :fail (fn [s2 e2]
                          (reply/ok! ctx s2 (f result) (error/merge error e2)))))))]
    (fn [state ctx]
      (reply/continue p state ctx
        :ok   ok-throw
        :ok!  (make-ok! ctx (f))
        :fail (fn [s e] (zero ctx s e))))))

(defn- zero-ok [ctx state error]
  (reply/ok ctx state nil error))

(defn- zero-fail [ctx state error]
  (reply/fail ctx state error))

(defn *
  "Zero or more."
  [p]
  (reduce-many `* zero-ok vec-rf p))

(defn +
  "One or more."
  [p]
  (reduce-many `+ zero-fail vec-rf p))

(defn skip* [p]
  (reduce-many `skip* zero-ok ignore p))

(defn skip+ [p]
  (reduce-many `skip+ zero-fail ignore p))

(defn max [p n]
  (cond
    (< n 1) pnil
    (= n 1) (? (pipe p list))
    :else   (? (pipe p (max (dec n) p) cons))))

;; fparsec: parray
(defn repeat
  {:arglists '([p n] [p min max])}
  ([p n]
   (sequence (core/repeat n p)))
  ([p min-n max-n]
   {:pre [(<= min-n max-n)]}
   (core/let [d (- max-n min-n)]
     (if (zero? d)
       (repeat p min-n)
       (cat (repeat p min-n) (max p d))))))

(defn min [p n]
  (cat (repeat p n) (* p)))

;; TODO: Skip variants

;; fparsec: sepBy, sepEndBy, manyTill + variants
;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Lazy / recursive

;; Note that we can use #'var as well
(defmacro lazy [& body]
  `(core/let [p# (delay ~@body)]
     (fn [state# ctx#]
       (reply/continue (force p#) state# ctx#))))

;; fparsec: createParserForwardedToRef
(defn ref
  "Returns a parser that forwards all calls to the parser returned
   by `@ref`.  Useful to construct recursive parsers."
  [ref]
  (fn [state ctx]
    (reply/continue @ref state ctx)))

(defn rec
  "Creates a recursive parser.  Calls `f` with one arg, a parser to recur,
   and returns the parser returned by `f`.

   It is assumed that `f` uses `p/alt` or similar to eventually stop the
   recursion."
  [f]
  (core/let [vol (volatile! pnil)]
    (vreset! vol (f (ref vol)))))

;;---------------------------------------------------------
;; Tokens

(def eof
  (fn [state ctx]
    (if (state/at-end? state)
      (reply/ok ctx state nil)
      (reply/fail ctx state error/expected-eof))))

;;---------------------------------------------------------
;; Handling state

(defn index
  "Returns the index of the next token in the input stream."
  [state ctx]
  (reply/ok ctx state (state/index state)))

(defn pos
  "Returns the current position in the input stream."
  [state ctx]
  (reply/ok ctx state (state/pos state)))

(defn user-state
  "Returns the current user state."
  [state ctx]
  (reply/ok ctx state (state/user-state state)))

(defn set-user-state
  "Sets the user state to `u`."
  [state ctx u]
  (reply/ok! ctx (state/with-user-state state u) u))

(defn swap-user-state
  "Sets ths user state to `(apply f user-state args)`."
  [state ctx f & args]
  (let [u (apply f (state/user-state state) args)]
    (reply/ok! ctx (state/with-user-state state u) u)))

(defn match-user-state
  "Succeeds if `pred` returns logical true when called with the current
   user state, otherwise it fails."
  [pred]
  (fn [state ctx]
    (if (pred (state/user-state state))
      (reply/ok ctx state nil)
      (reply/fail ctx state error/no-message))))

(comment
  ;; Idea: Coerce values to parser functions
  (defprotocol IParserCoercible
    (as-parser [_]))

  #_{:clj-kondo/ignore [:unresolved-namespace]}
  (extend-protocol IParserCoercible
    #?(:clj String :cljs js/String)
    (as-parser [s] (char/string s)))

  ;;
  )
