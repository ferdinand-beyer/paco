(ns comparse.core
  (:refer-clojure :exclude [cat char repeat])
  (:require [comparse.error :as error]
            [comparse.reply :as reply]
            [comparse.state :as state])
  #?(:require-macros [comparse.api :refer [with thunk]]))

(defn- parse-exception [state errors]
  (let [pos (state/position state)]
    (ex-info (str "Parse Error: " (error/render-errors errors)
                  " at " pos)
             {:type ::parse-error
              :position pos
              :errors errors})))

(def ^:private root-reply
  (reply/reply-fns
   (fn ok [_ result _] result)
   (fn error [state errors]
     (throw (parse-exception state errors)))))

(defn run [p input]
  ;; TODO: initial state from input + opts
  (trampoline p (state/of-string input) root-reply))

(defn- continue [p state reply]
  (fn [] (p state reply)))

;;---------------------------------------------------------

;; fparsec:
;; fparsec also has >>%: parse p, but return x
(defn return [x]
  (fn [state reply]
    (reply/ok reply state x)))

(def pnil
  (fn [state reply]
    (reply/ok reply state nil)))

;;---------------------------------------------------------
;; Chaining and piping

(defn- with-errors [reply errors]
  (if (seq errors)
    (letfn [(ok [s v e]
              (reply/ok reply s v (concat errors e)))
            (error [s e]
              (reply/error reply s (concat errors e)))]
      (reply/with reply :ok ok :error error))
    reply))

(defn bind [p f]
  (fn [state reply]
    (letfn [(ok [s v e]
              (continue (f v) s (with-errors reply e)))
            (ok! [s v _]
              (continue (f v) s (reply/with reply :ok :ok! :error :error!)))]
      (continue p state (reply/with reply :ok ok :ok! ok!)))))

(defn- emit-let [bindings body]
  (let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-let (drop 2 bindings) body))))))

(defmacro plet
  {:clj-kondo/lint-as 'clojure.core/let
   :style/indent 1}
  [bindings & body]
  (assert (vector? bindings) "with requires a vector for its binding")
  (assert (even? (count bindings)) "with requires an even number of forms in binding vector")
  (assert (some? body) "with requires a body")
  (emit-let bindings body))

;; fparsec: >>., .>>, .>>.
;; sequence of two parsers, returning either or both results.
;; .>>.: like (cat p1 p2)
;; parsesso: `>>.` is called `after`

(defn pseq*
  ([ps]
   (pseq* ps cons))
  ([ps f]
   (if-let [p (first ps)]
     (plet [x p, xs (pseq* (next ps) f)]
       (return (f x xs)))
     pnil)))

(defn pseq
  ([p1] p1)
  ([p1 p2]
   (pseq* (list p1 p2)))
  ([p1 p2 p3 & more]
   (pseq* (list* p1 p2 p3 more))))

;; fparsec: |>>, pipe2, pipe3, pipe4, pipe5
(defn pipe
  ([p f]
   (fn [state reply]
     (letfn [(ok [s v e]
               (reply/ok reply s (f v) e))
             (ok! [s v e]
               (reply/ok! reply s (f v) e))]
       (continue p state (reply/with reply :ok ok :ok! ok!)))))
  ([p1 p2 f]
   (pipe (pseq* (list p1 p2)) #(apply f %)))
  ([p1 p2 p3 & more]
   (let [args (list* p1 p2 p3 more)]
     (pipe (pseq* (butlast args)) #(apply (last args) %)))))

;;---------------------------------------------------------
;; Parsing alternatives and recovering from errors

(defn- backtracking
  "Returns a reply continuation that backtracks to `state`."
  [reply state]
  (letfn [(error! [s errors]
            (reply/error reply state
                         (error/nested (state/position s)
                                       (state/user-state s)
                                       errors)))]
    (reply/with reply :error! error!)))

;; TODO: Other fparsec-style backtracking operators
;; fparsec: >>=?, >>? (should be >>.?), .>>.?
;; These backtrack to the beginning if the second parser fails
;; "with a non‐fatal error and without changing the parser state"
;; alternate names: try, ptry, recover
(defn attempt [p]
  (fn [state reply]
    (continue p state (backtracking reply state))))

(defn- alt2
  ([p1 p2]
   (alt2 p1 p2 concat))
  ([p1 p2 errors-fn]
   (fn [state reply]
     (letfn [(error1 [s1 e1]
               (letfn [(ok2 [s2 v2 e2]
                         (reply/ok reply s2 v2 e2))
                       (error2 [s2 e2]
                         (reply/error reply s2 (errors-fn e1 e2)))]
                 (continue p2 s1 (reply/with reply :ok ok2 :error error2))))]
       (continue p1 state (reply/with reply :error error1))))))

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
(defn alt*
  ([ps]
   (if-let [p (first ps)]
     (reduce alt2 p (next ps))
     pnil))
  ([ps label]
   (if-let [p (first ps)]
     (let [errors-fn (constantly (error/expected label))]
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
  (let [errors (error/message message)]
    (fn [state reply]
      (reply/error reply state errors))))

;; fparsec: <?>
(defn expected
  "If `p` does not change the parser state, the errors are
   replaced with `(expected label)."
  [p label]
  (let [errors (error/expected label)]
    (fn [state reply]
      (letfn [(ok [s v _]
                (reply/ok reply s v errors))
              (error [s _]
                (reply/error reply s errors))]
        (continue p state (reply/with reply :ok ok :error error))))))

;; fparsec: <??> -- compound when p's reply is `error!`

;;---------------------------------------------------------
;; Sequences / seqexp

(defn- cat-cons [x xs]
  (cond
    (nil? x) xs
    (seq? x) (concat x xs)
    :else    (cons x xs)))

;; like pseq, but flattens
(defn cat
  ([p1] p1)
  ([p1 p2]
   (pseq* (list p1 p2) cat-cons))
  ([p1 p2 p3 & more]
   (pseq* (list* p1 p2 p3 more) cat-cons)))

;; alternative names: opt, maybe
(defn ?
  "Optional: zero or one."
  ([p]
   (alt2 p pnil))
  ([p not-found]
   (alt2 p (return not-found))))

;; TODO: Rename to just `*`
(defn p*
  "Zero or more."
  [p])

;; TODO: Rename to just `+`
(defn p+
  "One or more."
  [p])

(defn pmin [p min])
(defn pmax [p max])

(defn repeat
  ([p n])
  ([p min max]))

;; TODO: Skip variants

;; fparsec: sepBy, sepEndBy, manyTill + variants
;; fparsec: chainl, chainr, + variants

;;---------------------------------------------------------
;; Forward declaration / recursion

;; alternate names: do, do-p
(defmacro pdo [& body]
  `(fn [state# reply#]
     (continue (do ~@body) state# reply#)))

;; fparsec: createParserForwardedToRef
(defn pdelay
  "Returns `[p preset!]`, with the parser `p` forwarding all calls
   to an unrealized parser `q` that can later be set using `(preset! q)`.
   Useful to construct recursive parsers."
  []
  (let [pref (atom pnil)]
    (letfn [(p [state reply]
              (continue @pref state reply))
            (preset! [p]
              (reset! pref p))]
      (list p preset!))))

(defn rec
  "Creates a recursive parser.  Calls `f` with one arg, a parser to recur,
   and returns the parser returned by `f`.

   It is assumed that `f` uses `p/alt` or similar to eventually stop the
   recursion."
  [f]
  (let [[p preset!] (pdelay)]
    (preset! (f p))))

;;---------------------------------------------------------
;; Characters

;; fparsec: normalises newlines
;; alternate names: match
(defn satisfy
  ([pred]
   (satisfy pred nil))
  ([pred label]
   (let [error (if label
                 (let [e (first (error/expected label))]
                   (fn [reply state errors]
                     (reply/error reply state (cons e errors))))
                 reply/error)]
     (fn [state reply]
       (if-let [c (state/peek-char state)]
         (if (pred c)
           (reply/ok! reply (state/skip-char state) c)
           (error reply state (error/unexpected c)))
         (error reply state error/unexpected-eof))))))

(def eof
  (fn [state reply]
    (if (state/at-end? state)
      (reply/ok reply state nil)
      (reply/error reply state error/expected-eof))))

;; fparsec: pchar
;; fparsec: + skipChar, charReturn
(defn char [c]
  (satisfy #(= c %) c))

;; fparsec: + skipAnyChar
(def any-char
  (fn [state reply]
    (if-let [c (state/peek-char state)]
      (reply/ok! reply (state/skip-char state) c)
      (reply/error reply state error/unexpected-eof))))

;; fparsec: anyOf, noneOf
;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Construct a nicer error message
  (satisfy (set chars) (str chars)))

;; fparsec: asciiLower, asciiUpper, asciiLetter
;; fparsec: lower, upper, letter
;; fparsec: digit, hex, octal

;; fparsec: tab
;; fparsec: newline, skipNewline, newlineReturn + unicode variants
;; fparsec: spaces, spaces1 + unicode variants

;;---------------------------------------------------------
;; strings

;; fparsec: pstring
;; fparsec: + skipString, stringReturn, CI variants,
;;   anyString, skipAnyString
(defn string [s]
  {:pre [(seq s)
         (not (re-find #"[\r\n]" s))]}
  (let [length       (count s)
        errors (error/expected-str s)
        errors-eof (cons (first error/unexpected-eof) errors)]
    (fn [state reply]
      (if (state/matches? state s)
        (reply/ok! reply (state/skip* state length) s)
        (if (state/at-end? state)
          (reply/error reply state errors-eof)
          (reply/error reply state errors))))))

;; fparsec: restOfLine, skipRestOfLine
;; fparsec: charsTillString
#_(defn chars-till-str [s skip? max-count])

;; fparsec: manySatisfy
;; fparsec: manySatisfy2 -- different pred for first character
;; fparsec: many1Satisfy: one or more
;; fparsec: manyMinMaxSatisfy
#_(defn *match
  "Parses a sequence of zero or more characters satisfying `pred`,
   and returns them as a string."
  [pred])

;; fparsec: regex
;; fparsec: identifier

;; fparsec: combine parsers to return strings
;;   manyChars, manyStrings, skipped

;; fparsec: number parsers (int, float)

(comment
  (defprotocol IParserFactory
    (->parser [_]))

  (extend-protocol IParserFactory
    #?(:clj String :cljs js/String)
    (->parser [s]
      ;; Construct similar to p/word
      ))
;;
  )
