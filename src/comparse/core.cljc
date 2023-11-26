(ns comparse.core
  (:refer-clojure :exclude [* + cat max min ref repeat])
  (:require [clojure.core :as core]
            [comparse.error :as error]
            [comparse.state :as state])
  #?(:require-macros [comparse.core :refer [continue lazy with]]))

#?(:clj (set! *warn-on-reflection* true))

(defn- ok-result [state value msgs]
  {:status   :ok
   :value    value
   :position (state/position state)
   :user     (state/user-state state)
   :messages msgs})

(defn- ok-value [_ value _]
  {:pre [(not (fn? value))]}
  value)

(defn- fail-result [state msgs]
  {:status   :error
   :position (state/position state)
   :user     (state/user-state state)
   :messages msgs})

(defn- parser-exception [{:keys [position messages] :as result}]
  (ex-info (str "Parse Error: " (error/render-messages messages)
                " at " position)
           (assoc result :type ::parse-error)))

(defn- fail-exception [state msgs]
  (throw (parser-exception (fail-result state msgs))))

(defn- run-parser [p input ok fail]
  ;; TODO: initial state from input + opts
  (trampoline #(p (state/of-string input)
                  ok ok fail fail)))

(defn run [p input]
  (run-parser p input ok-result fail-result))

(defn parse [p input]
  (run-parser p input ok-value fail-exception))

;;---------------------------------------------------------

;; fparsec: preturn
;; fparsec also has >>%: parse p, but return x
(defn return [x]
  (fn [state ok _ _ _]
    (ok state x nil)))

;; fparsec: pzero, but fails
(defn pnil [state ok _ _ _]
  (ok state nil nil))

;;---------------------------------------------------------
;; Chaining and piping

(defmacro ^:private continue [p state ok ok! fail fail!]
  `(fn ~'cont [] (~p ~state ~ok ~ok! ~fail ~fail!)))

(defn- ok-with-msgs [ok msgs]
  (if (seq msgs)
    (fn [state value msgs']
      (ok state value (error/merge-messages msgs msgs')))
    ok))

(defn- fail-with-msgs [fail msgs]
  (if (seq msgs)
    (fn [state msgs']
      (fail state (error/merge-messages msgs msgs')))
    fail))

(defn bind [p f]
  (fn [state ok ok! fail fail!]
    (letfn [(ok' [state' value msgs]
              (continue (f value) state'
                        (ok-with-msgs ok msgs) ok!
                        (fail-with-msgs fail msgs) fail!))
            (ok!' [state' value msgs]
              (continue (f value) state'
                        (ok-with-msgs ok! msgs) ok!
                        (fail-with-msgs fail! msgs) fail!))]
      (continue p state ok' ok!' fail fail!))))

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
            (fn [result msgs state ok ok! fail fail!]
              (continue p state
                        (fn ok' [state' value' msgs']
                          (run-next (f result value')
                                    (error/merge-messages msgs msgs')
                                    state'
                                    ok ok!
                                    fail fail!))
                        (fn ok!' [state' value' msgs']
                          (run-next (f result value')
                                    msgs'
                                    state'
                                    ok! ok!
                                    fail! fail!))
                        (fail-with-msgs fail msgs)
                        fail!)))
          (complete [result msgs state ok _ _ _]
            (ok state (f result) msgs))
          (compile [ps]
            (if-let [p (first ps)]
              (run-fn p (compile (next ps)))
              complete))]
    (let [run (compile ps)]
      (fn [state ok ok! fail fail!]
        (run (f) nil state ok ok! fail fail!)))))

(defn- emit-pipe-body
  [f ps prev-state prev-values prev-msgs ok ok! fail fail!]
  (let [depth       (inc (count prev-values))
        make-sym    (fn [prefix] (symbol (str prefix depth)))
        state       (make-sym "state")
        value       (make-sym "value")
        msgs        (make-sym "msgs")
        values      (conj prev-values value)
        merged-msgs (cond->> msgs
                      prev-msgs (list `error/merge-messages prev-msgs))
        next-ps     (next ps)]
    (list `continue (first ps) prev-state
          (list `fn (make-sym "ok") [state value msgs]
                (if next-ps
                  (emit-pipe-body f next-ps state
                                  values merged-msgs
                                  ok ok!
                                  fail fail!)
                  (list ok state (cons f values) merged-msgs)))
          (list `fn (make-sym "ok!") [state value msgs]
                (if next-ps
                  (emit-pipe-body f next-ps state
                                  values msgs
                                  ok! ok!
                                  fail! fail!)
                  (list ok! state (cons f values) msgs)))
          (if prev-msgs
            (list `fn (make-sym "fail!") [state msgs]
                  (list fail state merged-msgs))
            fail)
          fail!)))

(defn- emit-pipe-parser [ps f]
  (list `fn (symbol (str "pipe" (count ps)))
        ['state 'ok 'ok! 'fail 'fail!]
        (emit-pipe-body f ps 'state [] nil
                        'ok 'ok! 'fail 'fail!)))

(defmacro ^:private pipe-parser [& ps+f]
  (emit-pipe-parser (butlast ps+f) (last ps+f)))

(comment
  (emit-pipe-parser '[p1 p2 p3 p4 p5] 'f)
  ;
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
;; Would be cool to call this `do`, but this fails
(defn >> [& ps]
  (reduce-series (fn
                   ([] nil)
                   ([result] result)
                   ([_ input] input))
                 ps))

;; fparsec: .>> - return the first result
;; fparsetc: .>>.: like (cat p1 p2)
;; fparsec: between

;;---------------------------------------------------------
;; Parsing alternatives and recovering from errors

(defn- nested-error [state msgs]
  (error/nested (state/position state)
                (state/user-state state)
                msgs))

(defn- ok-backtrack [ok state value]
  (fn [state' msgs]
    (ok state value (nested-error state' msgs))))

(defn- fail-backtrack [fail state]
  (fn [state' msgs]
    (fail state (nested-error state' msgs))))

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover, !
(defn attempt [p]
  (fn [state ok ok! fail _]
    (continue p state ok ok! fail (fail-backtrack fail state))))

(defn ??
  "Applies `p`.  If `p` fails, `??` will backtrack to the original state
   and succeed with `not-found` or `nil`.

   `(?? p)` is an optimized implementation of `(? (attempt p))`."
  ([p] (?? p nil))
  ([p not-found]
   (fn [state ok ok! _ _]
     (continue p state ok ok!
               (fn [state msgs]
                 (ok state not-found msgs))
               (ok-backtrack ok state not-found)))))

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 error/merge-messages))
  ([p1 p2 merge-msgs]
   (fn [state ok ok! fail fail!]
     (letfn [(fail1 [state1 msgs1]
               (letfn [(ok2 [state2 value2 msgs2]
                         (ok state2 value2 (merge-msgs msgs1 msgs2)))
                       (fail2 [state2 msgs2]
                         (fail state2 (merge-msgs msgs1 msgs2)))]
                 (continue p2 state1 ok2 ok! fail2 fail!)))]
       (continue p1 state ok ok! fail1 fail!)))))

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
     (core/let [errors-fn (constantly (error/expected label))]
       (reduce #(alt2 %1 %2 errors-fn) p (next ps)))
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
  (core/let [msgs (error/message message)]
    (fn [state _ _ fail _]
      (fail state msgs))))

;; fparsec: <?>
(defn expected
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (core/let [msgs (error/expected label)]
    (fn [state ok ok! fail fail!]
      (letfn [(ok' [state' value _]
                (ok state' value msgs))
              (fail' [state' _]
                (fail state' msgs))]
        (continue p state ok' ok! fail' fail!)))))

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
   `(zero state msgs ok fail)`, so that implementations
   can succeed or fail."
  [sym zero f p]
  (letfn [(ok-throw [_ _ _]
            (throw (state-unchanged-exception sym p)))]
    (fn [state ok ok! fail fail!]
      (letfn [(fail0 [state' msgs]
                (zero state' msgs ok fail))
              (make-ok! [result]
                (fn [state value msgs]
                  (core/let [result (f result value)
                             fail' (fn [state' msgs']
                                     (ok! state' (f result) (error/merge-messages msgs msgs')))]
                    (continue p state ok-throw (make-ok! result) fail' fail!))))]
        (continue p state ok-throw (make-ok! (f)) fail0 fail!)))))

(defn- zero-ok [state msgs ok _]
  (ok state nil msgs))

(defn- zero-fail [state msgs _ fail]
  (fail state msgs))

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
    (< n 1) empty
    (= n 1) (? (pipe p list))
    :else   (? (pipe p (max (dec n) p) cons))))

;; fparsec: parray
(defn repeat
  {:arglists '([p n] [p min max])}
  ([p n]
   (series (core/repeat n p)))
  ([p min max-n]
   {:pre [(<= min max-n)]}
   (core/let [d (- max-n min)]
     (if (zero? d)
       (repeat p min)
       (cat (repeat p min) (max p d))))))

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
     (fn [state# ok# ok!# fail# fail!#]
       (continue (force p#) state# ok# ok!# fail# fail!#))))

;; fparsec: createParserForwardedToRef
(defn ref
  "Returns a parser that forwards all calls to the parser returned
   by `@ref`.  Useful to construct recursive parsers."
  [ref]
  (fn [state ok ok! fail fail!]
    (continue @ref state ok ok! fail fail!)))

(defn mut
  "Returns a mutable parser that forwards all calls to an encapsulated
   parser `p`.  Call the returned parser function with one arg to reset
   `p`.  Useful to construct recursive parsers."
  ([]
   (mut pnil))
  ([p]
   (core/let [ref (atom p)]
     (fn
       ([p'] (reset! ref p'))
       ([state ok ok! fail fail!]
        (continue @ref state ok ok! fail fail!))))))

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

;; fparsec: normalises newlines
;; alternate names: match
(defn satisfy
  ([pred]
   (satisfy pred nil))
  ([pred label]
   (core/let [wrap-msgs (if label
                          (core/let [msg (first (error/expected label))]
                            #(cons msg %))
                          identity)]
     (fn [state _ ok! fail _]
       (if-let [token (state/peek-char state)]
         (if (pred token)
           (ok! (state/skip-char state) token nil)
           (fail state (wrap-msgs (error/unexpected token))))
         (fail state (wrap-msgs error/unexpected-eof)))))))

(def eof
  (fn [state ok _ fail _]
    (if (state/at-end? state)
      (ok state nil nil)
      (fail state error/expected-eof))))

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
