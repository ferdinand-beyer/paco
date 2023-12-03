(ns paco.core
  (:refer-clojure :exclude [* + cat max min ref repeat sequence])
  (:require [clojure.core :as core]
            [paco.detail :as detail]
            [paco.error :as error]
            [paco.state :as state])
  #?(:cljs (:require-macros [paco.core :refer [pipe-parser]])))

(defn- result [status state value error]
  {:status   status
   :value    value
   :position (state/pos state)
   :user     (state/user-state state)
   :messages (error/sort-messages error)})

(defn- parser-exception [status state value error]
  (let [result (result status state value error)]
    (ex-info (error/string error (:position result))
             (assoc result :type ::parse-error))))

(defn run [p input & {:as opts}]
  (detail/run-parser p (state/of input opts) result))

(defn parse [p input & {:as opts}]
  (letfn [(reply [status state value error]
            (if (detail/ok? status)
              value
              (throw (parser-exception status state value error))))]
    (detail/run-parser p (state/of input opts) reply)))

;;---------------------------------------------------------

;; fparsec: preturn
;; fparsec also has >>%: parse p, but return x
(defn return
  ([x]
   (fn [state reply]
     (reply detail/ok state x nil)))
  ([p x]
   (fn [state reply]
     (letfn [(reply1 [status state _value error]
               (reply status state x error))]
       (detail/thunk (p state reply1))))))

;; fparsec: pzero, but fails
(defn pnil
  "This parser always succeeds and returns `nil`."
  [state reply]
  (reply detail/ok state nil nil))

(defn eof [state reply]
  (if (state/at-end? state)
    (reply detail/ok state nil nil)
    (reply detail/fail state nil error/expected-eof)))

;;---------------------------------------------------------
;; Chaining and piping

(defn bind [p f]
  (fn [state reply]
    (letfn [(reply1 [status1 state1 value1 error1]
              (if (detail/ok? status1)
                (let [p2 (f value1)]
                  (if (nil? error1)
                    (detail/thunk (p2 state1 reply))
                    (letfn [(reply2 [status2 state2 value2 error2]
                              (if (detail/same-state? state1 state2)
                                (reply status2 state2 value2 (error/merge error1 error2))
                                (reply status2 state2 value2 error2)))]
                      (detail/thunk (p2 state1 reply2)))))
                (reply status1 state1 value1 error1)))]
      (detail/thunk (p state reply1)))))

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

(defn- emit-pipe-body
  [f ps prev-state prev-reply prev-values prev-error]
  (let [depth    (inc (count prev-values))
        make-sym (fn [prefix] (symbol (str prefix depth)))
        reply    (make-sym "reply")
        status   (make-sym "status")
        state    (make-sym "state")
        value    (make-sym "value")
        error    (make-sym "error")
        e        (make-sym "e")
        values   (conj prev-values value)
        next-ps  (next ps)]
    `(letfn [(~reply [~status ~state ~value ~error]
               (let [~e ~(if prev-error
                           `(detail/pass-error ~state ~error ~prev-state ~prev-error)
                           error)]
                 ~(if next-ps
                    (emit-pipe-body f next-ps state prev-reply values e)
                    (list prev-reply status state
                          `(if (detail/ok? ~status)
                             ~(cons f values)
                             ~value)
                          e))))]
       (detail/thunk (~(first ps) ~prev-state ~reply)))))

(defn- emit-pipe-parser [ps f]
  (list `fn (symbol (str "pipe" (count ps))) '[state reply]
        (emit-pipe-body f ps 'state 'reply [] nil)))

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
     (detail/reduce-sequence (completing conj #(apply f %)) ps))))

;; alternative names: g, group', groups, pseq, tuples
(defn sequence
  "The parser `(series ps)` applies the parsers `ps` in sequence."
  [ps]
  (detail/reduce-sequence detail/vector-rf ps))

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
;; Would be cool to call this `do`, but this fails for (do)
;; since it is a special form (paco.core/do) ;=> nil
;; alternative names: then, pdo
(defn >> [& ps]
  (detail/reduce-sequence detail/then-rf ps))

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
  (fn [state reply]
    (letfn [(reply1 [status1 state1 value1 error1]
              (cond
                (detail/ok? status1)
                (reply status1 state1 value1 error1)

                (not (detail/same-state? state1 state))
                (reply detail/fail state nil (error/nested state1 error1))

                (detail/fatal? status1) (reply detail/fail state1 value1 error1)

                :else (reply status1 state1 value1 error1)))]
      (detail/thunk (p state reply1)))))

;; or: ?!, ?try
;; not to be confused with regex negative look-ahead (?!...)
(defn ?!
  "Applies `p`.  If `p` fails, `?!` will backtrack to the original state
   and succeed with `not-found` or `nil`.

   `(?! p)` is an optimized implementation of `(? (attempt p))`."
  ([p] (?! p nil))
  ([p not-found]
   (fn [state reply]
     (letfn [(reply1 [status1 state1 value1 error1]
               (if (detail/fail? status1)
                 (reply detail/ok state not-found (if (detail/same-state? state1 state)
                                                    error1
                                                    (error/nested state1 error1)))
                 (reply status1 state1 value1 error1)))]
       (detail/thunk (p state reply1))))))

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 error/merge))
  ([p1 p2 merge-errors]
   (fn [state reply]
     (letfn [(reply1 [status1 state1 value1 error1]
               (if (detail/ok? status1)
                 (reply status1 state1 value1 error1)
                 (if (nil? error1)
                   (detail/thunk (p2 state reply))
                   (letfn [(reply2 [status2 state2 value2 error2]
                             (reply status2 state2 value2 (if (detail/same-state? state1 state2)
                                                            (merge-errors error1 error2)
                                                            error2)))]
                     (detail/thunk (p2 state reply2))))))]
       (detail/thunk (p1 state reply1))))))

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

;; names: ?=, ?!, ?<=, ?<!

;;---------------------------------------------------------
;; Customizing error messages

(defn fail [message]
  (core/let [error (error/message message)]
    (fn [state reply]
      (reply detail/fail state nil error))))

;; fparsec: <?>
(defn as
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (core/let [error (error/expected label)]
    (fn [state reply]
      (letfn [(reply1 [status1 state1 value1 error1]
                (reply status1 state1 value1
                       (if (detail/same-state? state1 state)
                         error
                         error1)))]
        (detail/thunk (p state reply1))))))

;; fparsec: <??>
(defn as! [p label]
  (core/let [error (error/expected label)]
    (fn [state reply]
      (letfn [(reply1 [status1 state1 value1 error1]
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
                  (reply detail/fatal state nil (error/compound label state1 error1))))]
        (detail/thunk (p state reply1))))))

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

;; fparsec: sepBy, sepEndBy, manyTill + variants
;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Lazy / recursive

(defn- lazy* [dp]
  (fn [state reply]
    (let [p (force dp)]
      (detail/thunk (p state reply)))))

;; Note that we can use #'var as well
(defmacro lazy [& body]
  `(lazy* (delay ~@body)))

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
  (core/let [vol (volatile! pnil)]
    (vreset! vol (f (ref vol)))))

;;---------------------------------------------------------
;; Handling state

(defn index
  "Returns the index of the next token in the input stream."
  [state reply]
  (reply detail/ok state (state/index state) nil))

(defn pos
  "Returns the current position in the input stream."
  [state reply]
  (reply detail/ok state (state/pos state) nil))

(defn user-state
  "Returns the current user state."
  [state reply]
  (reply detail/ok state (state/user-state state) nil))

(defn set-user-state
  "Sets the user state to `u`."
  [u]
  (fn [state reply]
    (reply detail/ok (state/with-user-state state u) u nil)))

(defn swap-user-state
  "Sets ths user state to `(apply f user-state args)`."
  [f & args]
  (fn [state reply]
    (let [u (apply f (state/user-state state) args)]
      (reply detail/ok (state/with-user-state state u) u nil))))

(defn match-user-state
  "Succeeds if `pred` returns logical true when called with the current
   user state, otherwise it fails."
  ([pred]
   (match-user-state pred nil))
  ([pred value]
   (fn [state reply]
     (if (pred (state/user-state state))
       (reply detail/ok state value nil)
       (reply detail/fail state nil error/no-message)))))

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
