(ns comparse.core
  (:refer-clojure :exclude [* + cat max min ref repeat])
  (:require [clojure.core :as core]
            [comparse.error :as error]
            [comparse.state :as state])
  #?(:require-macros [comparse.core :refer [continue lazy with]]))

#?(:clj (set! *warn-on-reflection* true))

(defn- parser-ok [_state value _msgs]
  {:pre [(not (fn? value))]}
  value)

(defn- parse-exception [state msgs]
  (core/let [pos (state/position state)]
    (ex-info (str "Parse Error: " (error/render-messages msgs)
                  " at " pos)
             {:type ::parse-error
              :position pos
              :messages msgs})))

(defn- parser-fail [state msgs]
  ;(throw (parse-exception state msgs))
  (parse-exception state msgs))

(defn run [p input]
  ;; TODO: initial state from input + opts
  (trampoline #(p (state/of-string input)
                  parser-ok parser-ok
                  parser-fail parser-fail)))

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
      (ok state value (concat msgs msgs')))
    ok))

(defn- fail-with-msgs [fail msgs]
  (if (seq msgs)
    (fn [state msgs']
      (fail state (concat msgs msgs')))
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

;; TODO: Implement like * combinator, with a reducing function
(defn series
  "The parser `(series ps)` applies the parsers `ps` in sequence."
  ([ps]
   (series ps cons))
  ([ps f]
   (if-let [p (first ps)]
     (with [x p, xs (series (next ps) f)]
       (return (f x xs)))
     pnil)))

(defn group
  ([p1] p1)
  ([p1 p2]
   (series (list p1 p2)))
  ([p1 p2 p3 & more]
   (series (list* p1 p2 p3 more))))

;; fparsec: |>>, pipe2, pipe3, pipe4, pipe5
(defn pipe
  ([p f]
   (letfn [(wrap-ok [ok]
             (fn [state value msgs]
               (ok state (f value) msgs)))]
     (fn [state ok ok! fail fail!]
       (continue p state (wrap-ok ok) (wrap-ok ok!) fail fail!))))
  ([p1 p2 f]
   (pipe (group p1 p2) #(f (first %) (second %))))
  ([p1 p2 p3 & more]
   (core/let [args (list* p1 p2 p3 more)]
     (pipe (series (butlast args)) #(apply (last args) %)))))

(defn- emit-do* [[p & ps]]
  (if (seq ps)
    `(bind ~p (fn [~'_] ~(emit-do* ps)))
    p))

(defn- emit-do [ps]
  (if (seq ps)
    (emit-do* ps)
    `pnil))

;; fparsec: >>.
;; parsesso: after
;; Would be cool to call this `do`, but this fails
(defmacro >> [& body]
  (emit-do body))

;; fparsec: >>., .>>, .>>.
;; sequence of two parsers, returning either or both results.
;; .>>.: like (cat p1 p2)
;; fparsec: between

(comment
  (run (>> (return 1) (return 2)) "")

  )

;;---------------------------------------------------------
;; Parsing alternatives and recovering from errors

(defn- fail-backtrack [fail state]
  (fn [state' msgs]
    (fail state (error/nested (state/position state')
                              (state/user-state state')
                              msgs))))

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover
(defn attempt [p]
  (fn [state ok ok! fail _]
    (continue p state ok ok! fail (fail-backtrack fail state))))

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 concat))
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

(defn- cat-cons [x xs]
  (cond
    (nil? x) xs
    (seq? x) (concat x xs)
    :else    (cons x xs)))

;; like group, but flattens
(defn cat
  ([p1] p1)
  ([p1 p2]
   (series (list p1 p2) cat-cons))
  ([p1 p2 p3 & more]
   (series (list* p1 p2 p3 more) cat-cons)))

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

;; Ideas:
;; - generalise this to take a sequence of parsers.
;;   so that we can also use that for pseq*
;; - for many: use (repeat p'), wrapping p with a parser
;;   that throws on 'ok', so that this detail can be contained
;; - use `loop` and `run` each parser with a `reply`,
;;   instead of using continuation-passing style (benchmark!)

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
                                     (ok! state' (f result) (concat msgs msgs')))]
                    (continue p state ok-throw (make-ok! result) fail' fail!))))]
        (continue p state ok-throw (make-ok! (f)) fail0 fail!)))))

(defn- zero-ok [state msgs ok _]
  (ok state nil msgs))

(defn- zero-fail [state msgs _ fail]
  (fail state msgs))

(defn- accumulate
  "Reducing function that collects input in a vector.
   Like `conj!`, but completes with a persistent collection."
  ([] (transient []))
  ([coll] (persistent! coll))
  ([coll x] (conj! coll x)))

(defn *
  "Zero or more."
  [p]
  (reduce-many `* zero-ok accumulate p))

(defn +
  "One or more."
  [p]
  (reduce-many `+ zero-fail accumulate p))

(defn max [p n]
  (cond
    (< n 1) empty
    (= n 1) (? (pipe p list))
    :else   (? (pipe p (max (dec n) p) cons))))

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
