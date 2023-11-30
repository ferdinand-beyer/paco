(ns paco.core
  (:refer-clojure :exclude [* + cat max min ref repeat])
  (:require [clojure.core :as core]
            [paco.error :as error]
            [paco.reply :as reply]
            [paco.state :as state])
  #?(:require-macros [paco.core :refer [continue lazy with]]))

#?(:clj (set! *warn-on-reflection* true))

(defn- ok-result [state value error]
  {:status   :ok
   :value    value
   :position (state/position state)
   :user     (state/user-state state)
   :messages (error/sort-messages error)})

(defn- ok-value [_ value _]
  {:pre [(not (fn? value))]}
  value)

(defn- fail-result [state error]
  {:status   :error
   :position (state/position state)
   :user     (state/user-state state)
   :messages (error/sort-messages error)})

(defn- parser-exception [{:keys [position messages] :as result}]
  (ex-info (error/string messages position)
           (assoc result :type ::parse-error)))

(defn- fail-exception [state error]
  (throw (parser-exception (fail-result state error))))

(defn- run-parser [p input _opts ok fail]
  ;; TODO: initial state from input + opts
  (trampoline #(p (state/of-string input) (reply/context ok fail))))

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
       :ok  (fn [s _ m] (reply/ok ctx s x m))
       :ok! (fn [s _ m] (reply/ok! ctx s x m))))))

;; fparsec: pzero, but fails
(defn pnil [state ctx]
  (reply/ok ctx state nil))

;;---------------------------------------------------------
;; Chaining and piping

(defn bind [p f]
  (fn [state ctx]
    (reply/continue p state ctx
      :ok (fn [s1 v1 m1]
            (let [p2 (f v1)]
              (reply/continue p2 s1 ctx
                :ok   (reply/fwd-ok ctx m1)
                :fail (reply/fwd-fail ctx m1))))
      :ok! (fn [s1 v1 m1]
             (let [p2 (f v1)]
               (reply/continue p2 s1 ctx
                 :ok   (reply/fwd-ok! ctx m1)
                 :fail (reply/fwd-fail! ctx m1)))))))

(defn- emit-let [bindings body]
  (core/let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      ;; TODO Emit "do" body?
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-let (drop 2 bindings) body))))))

;; TODO: Rename to `let`
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
;; TODO: Support reduced?
(defn- reduce-series
  "Creates a parser that runs `ps` in sequence, reducing their return
   values with the reducing function `f`."
  [f ps]
  (letfn [(run-fn [p run-next]
            (fn [result msg state ctx]
              (reply/continue p state ctx
                :ok   (fn [s1 v1 m1]
                        (run-next (f result v1) (error/union msg m1) s1 ctx))
                :ok!  (fn [s1 v1 m1]
                        (run-next (f result v1) m1 s1 (reply/with ctx, :ok :ok!, :fail :fail!)))
                :fail (fn fail [s1 m1]
                        (reply/fail ctx s1 (error/union msg m1))))))
          (complete [result msg state ctx]
            (reply/ok ctx state (f result) msg))
          (compile [ps]
            (if-let [p (first ps)]
              (run-fn p (compile (next ps)))
              complete))]
    (let [run (compile ps)]
      (fn [state ctx]
        (run (f) nil state ctx)))))

(defn- emit-pipe-body
  [f ps prev-state prev-values prev-msg ctx]
  (let [depth       (inc (count prev-values))
        make-sym    (fn [prefix] (symbol (str prefix depth)))
        state       (make-sym "state")
        value       (make-sym "value")
        msg         (make-sym "msg")
        values      (conj prev-values value)
        merged-msg  (cond->> msg
                      prev-msg (list `error/union prev-msg))
        next-ps     (next ps)]
    (list* `reply/continue (first ps) prev-state ctx
           :ok (list `fn (make-sym "ok") [state value msg]
                     (if next-ps
                       (emit-pipe-body f next-ps state
                                       values merged-msg
                                       ctx)
                       (list `reply/ok ctx state (cons f values) merged-msg)))
           :ok! (list `fn (make-sym "ok!") [state value msg]
                      (if next-ps
                        (emit-pipe-body f next-ps state
                                        values msg
                                        `(reply/with ~ctx, :ok :ok!, :fail :fail!))
                        (list `reply/ok! ctx state (cons f values) msg)))
           (when prev-msg
             (list :fail (list `reply/fwd-fail ctx prev-msg))))))

(defn- emit-pipe-parser [ps f]
  (list `fn (symbol (str "pipe" (count ps))) '[state ctx]
        (emit-pipe-body f ps 'state [] nil 'ctx)))

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
     (reduce-series (completing conj #(apply f %)) ps))))

;; alternative names: g, group', groups, pseq
(defn series
  "The parser `(series ps)` applies the parsers `ps` in sequence."
  [ps]
  (reduce-series vec-rf ps))

;; fparsec: .>>., tuple2, tuple3, tuple4, tuple5
(defn group
  ([p] p)
  ([p1 p2]
   (pipe p1 p2 list))
  ([p1 p2 p3]
   (pipe p1 p2 p3 list))
  ([p1 p2 p3 p4 & more]
   (series (list* p1 p2 p3 p4 more))))

;; fparsec: >>.
;; parsesso: after
;; Would be cool to call this `do`, but this fails for (do)
;; since it is a special form
(defn >> [& ps]
  (reduce-series (fn
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

(defn- nested-error [state msg]
  (error/nested (state/position state)
                (state/user-state state)
                msg))

(defn- ok-backtrack [ctx state value]
  (fn [state' msg]
    (reply/ok ctx state value (nested-error state' msg))))

(defn- fail-backtrack [ctx state]
  (fn [state' msg]
    (reply/fail ctx state (nested-error state' msg))))

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover, !
(defn attempt [p]
  (fn [state ctx]
    (reply/continue p state ctx
      :fail! (fail-backtrack ctx state))))

(defn ??
  "Applies `p`.  If `p` fails, `??` will backtrack to the original state
   and succeed with `not-found` or `nil`.

   `(?? p)` is an optimized implementation of `(? (attempt p))`."
  ([p] (?? p nil))
  ([p not-found]
   (fn [state ctx]
     (reply/continue p state ctx
       :fail  (fn fail [state msg]
                (reply/ok ctx state not-found msg))
       :fail! (ok-backtrack ctx state not-found)))))

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 error/union))
  ([p1 p2 msg-union]
   (fn [state ctx]
     (reply/continue p1 state ctx
       :fail (fn fail [s1 m1]
               (reply/continue p2 s1 ctx
                 :ok   (fn ok [s2 v2 m2]
                         (reply/ok ctx s2 v2 (msg-union m1 m2)))
                 :fail (fn fail [s2 m2]
                         (reply/fail ctx s2 (msg-union m1 m2)))))))))

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
     (core/let [msg-fn (constantly (error/expected label))]
       (reduce #(alt2 %1 %2 msg-fn) p (next ps)))
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
  (core/let [msg (error/message message)]
    (fn [state ctx]
      (reply/fail ctx state msg))))

;; fparsec: <?>
(defn expected
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (core/let [msg (error/expected label)]
    (fn [state ctx]
      (reply/continue p state ctx
        :ok   (fn [s1 v1 _]
                (reply/ok ctx s1 v1 msg))
        :fail (fn [s1 _]
                (reply/fail ctx s1 msg))))))

;; fparsec: <??> -- compound when p's reply is `fail!` (name `expected!`?)

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
  (reduce-series cat-rf ps))

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
   `(zero ctx state msg)`, so that implementations
   can succeed or fail."
  [sym zero f p]
  (letfn [(ok-throw [_ _ _]
            (throw (state-unchanged-exception sym p)))
          (make-ok! [ctx result]
            (fn [state value msg]
              (core/let [result (f result value)]
                (reply/continue p state ctx
                  :ok   ok-throw
                  :ok!  (make-ok! ctx result)
                  :fail (fn [s2 m2]
                          (reply/ok! ctx s2 (f result) (error/union msg m2)))))))]
    (fn [state ctx]
      (reply/continue p state ctx
        :ok   ok-throw
        :ok!  (make-ok! ctx (f))
        :fail (fn [s m] (zero ctx s m))))))

(defn- zero-ok [ctx state msg]
  (reply/ok ctx state nil msg))

(defn- zero-fail [ctx state msg]
  (reply/fail ctx state msg))

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
   (series (core/repeat n p)))
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
      (reply/ok ctx state nil nil)
      (reply/fail ctx state error/expected-eof))))

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
