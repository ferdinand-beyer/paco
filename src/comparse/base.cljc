(ns comparse.base
  (:refer-clojure :exclude [cat])
  (:require [comparse.error :as error]
            [comparse.reply :as reply]
            [comparse.state :as state])
  #?(:require-macros [comparse.api :refer [with]]))

(defn return [x]
  (fn [state]
    (reply/success state x)))

(defn fail [message]
  (fn [state]
    (reply/failure state (list (error/message message)))))

(defn pipe
  ([p f]
   (fn [state1]
     (reply/then
      (p state1)
      (fn [state2 value]
        (reply/success state2 (f value))))))
  ([p1 p2 f]
   (fn [state1]
     (reply/then
      (p1 state1)
      (fn [state2 value1]
        (reply/then
         (p2 state2)
         (fn [state3 value2]
           (reply/success state3 (f value1 value2)))))))))

(defn bind [p f]
  (fn [state1]
    (reply/then (p state1)
                (fn [state2 value]
                  ((f value) state2)))))

(defn- emit-with [bindings body]
  (let [[binding p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~binding] ~@body))
      `(bind ~p (fn [~binding] ~(emit-with (drop 2 bindings) body))))))

(defmacro with
  {:clj-kondo/lint-as 'clojure.core/let
   :style/indent 1}
  [bindings & body]
  (assert (vector? bindings) "with requires a vector for its binding")
  (assert (even? (count bindings)) "with requires an even number of forms in binding vector")
  (assert (some? body) "with requires a body")
  (emit-with bindings body))

;; TODO: Other fparsec-style backtracking operators
(defn attempt [p]
  (fn [state1]
    (let [reply (p state1)]
      (reply/else
       reply
       (fn [state2 errors]
         (if (state/changed? state1 state2)
           ;; Backtrack
           (reply/failure state1
                          (list
                           (error/nested (state/position state2)
                                         (state/user-state state2)
                                         errors)))
           reply))))))

;; TODO: Can we implement that without recursion?
;; Similar to pipe
(defn cat* [ps]
  (if-let [p (first ps)]
    (with [x p, xs (cat* (next ps))]
      (return (cons x xs)))
    (return nil)))

(defn cat
  ([p1 p2]
   (pipe p1 p2 list))
  ([p1 p2 p3 & more]
   (cat* (list* p1 p2 p3 more))))

;; fparsec: satisfy
(defn match
  ([pred]
   (match pred nil))
  ([pred label]
   (let [errors (if label
                  #(list % (error/expected label))
                  #(list %))]
     (fn [state]
       (if-let [c (state/peek-char state)]
         (if (pred c)
           (reply/success (state/skip-char state) c)
           (reply/failure state (errors (error/unexpected c))))
         (reply/failure state (errors error/unexpected-eof)))))))

(def eof
  (let [errors (list (error/expected "end of input"))]
    (fn [state]
      (if (state/at-end? state)
        (reply/success state nil)
        (reply/failure state errors)))))

(defn- error [reply]
  (let [pos    (-> reply reply/state state/position)
        errors (reply/errors reply)]
    ;; TODO: Position to string
    (throw (ex-info (str "Parse Error: " (error/render-errors errors))
                    {:type ::parse-error
                     :position pos
                     :errors errors}))))

(defn- result [reply]
  (if (reply/success? reply)
    (reply/result reply)
    (error reply)))

(defn run [p input]
  ;; TODO: initial state from input + opts
  ;; TODO: ParserResult
  ;; TODO: Continuation-Loop (limit stack depth)
  (result (p (state/of-string input))))

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
