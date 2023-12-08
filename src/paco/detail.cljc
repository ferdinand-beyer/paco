(ns paco.detail
  (:require [paco.error :as error]
            [paco.state :as state])
  #?(:cljs (:require-macros [paco.detail :refer [call same-state? thunk]])))

(defn ok? [status]
  (#?(:clj identical?, :cljs keyword-identical?) :ok status))

(defn fail? [status]
  (not (#?(:clj identical?, :cljs keyword-identical?) :ok status)))

(defn error? [status]
  (#?(:clj identical?, :cljs keyword-identical?) :error status))

(defn fatal? [status]
  (#?(:clj identical?, :cljs keyword-identical?) :fatal status))

(defmacro same-state? [state other]
  `(identical? ~state ~other))

(defn merge-errors
  ([e1 s1 e0 s0]
   (if (same-state? s1 s0)
     (error/merge e1 e0)
     e1))
  ([e2 s2 e1 s1 e0 s0]
   (if (same-state? s2 s1)
     (error/merge e2 (if (same-state? s1 s0)
                       (error/merge e1 e0)
                       e1))
     e2)))

(defmacro thunk [& body]
  `(fn [] ~@body))

(defn run-parser [p state reply]
  (trampoline #(p state reply)))

(defmacro call [p state reply]
  `(let [reply# ~reply] (thunk (~p ~state reply#))))

;;---------------------------------------------------------
;; Reducing functions

(def ignore
  "Ignores all args and returns `nil`.  Useful for skip parsers."
  (constantly nil))

(defn vector-rf
  "Reducing function that collects input in a vector.
   Like `conj!`, but completes with a persistent collection."
  ([] (transient []))
  ([coll] (persistent! coll))
  ([coll x] (conj! coll x)))

(defn last-rf
  "Reducing function that only keeps the last input."
  ([] nil)
  ([result] result)
  ([_ input] input))

(defn first-rf
  "Reducing function that only keeps the first input."
  ([] nil)
  ([result] (first result))
  ([result input] (if (nil? result)
                    (list input)
                    result)))

(def ^:private ^:const seqexp-tag ::seqexp)
(def ^:private ^:const seqexp-meta {seqexp-tag true})

(defn seqexp-rf
  "Reducing function for 'sequence expression' parsers."
  ([] (transient []))
  ([xs]
   (-> xs
       persistent!
       (with-meta seqexp-meta)))
  ([xs x]
   (if (nil? x)
     xs
     (if (contains? (meta x) seqexp-tag)
       (reduce conj! xs x)
       (conj! xs x)))))

;;---------------------------------------------------------
;; Advanced parsers

(defn reduce-sequence
  "Creates a parser that runs `ps` in sequence, reducing their return
   values with the reducing function `rf`."
  [rf ps]
  (letfn [(step-fn [p step]
            (fn [reply state acc error]
              (call p state (fn [status state1 value error1]
                              (if (ok? status)
                                (step reply state1 (rf acc value) (merge-errors error1 state1 error state))
                                (reply status state1 value (merge-errors error1 state1 error state)))))))
          (complete [reply state acc error]
            (reply :ok state (rf acc) error))
          (compile [ps]
            (if-let [p (first ps)]
              (step-fn p (compile (next ps)))
              complete))]
    (let [step (compile ps)]
      (fn [state reply]
        (step reply state (rf) nil)))))

(defn- infinite-loop-exception [sym p state]
  (ex-info (str "Parser supplied to '" sym "' succeeded without changing the parser state")
           {:type ::infinite-loop
            :parser p
            :combinator sym
            :pos (state/pos state)}))

(defn reduce-repeat
  ([sym p rf min]
   (reduce-repeat sym p rf min nil))
  ([sym p rf min max]
   {:pre [(nat-int? min) (or (nil? max) (<= min max))]}
   (letfn [(step [reply acc n state1 error1]
             (call p state1 (fn [status state2 value error2]
                              (if (ok? status)
                                (if (same-state? state1 state2)
                                  (throw (infinite-loop-exception sym p state1))
                                  (let [acc (rf acc value)
                                        n   (inc n)]
                                    (if (or (nil? max) (< n max))
                                      (step reply acc n state2 error2)
                                      (reply :ok state2 (rf acc) error2))))
                                (let [err (if (and error1 (same-state? state1 state2))
                                            (error/merge error1 error2)
                                            error2)]
                                  (if (< n min)
                                    (reply :error state2 nil err)
                                    (reply :ok state2 (rf acc) err)))))))]
     (if (and max (zero? max))
       (fn [state reply]
         (reply :ok state (rf (rf)) nil))
       (fn [state reply]
         (step reply (rf) 0 state nil))))))

(defn reduce-sep
  [sym p sep rf empty-ok? sep-end-ok?]
  (letfn [(step [reply acc state0 error0]
            (call sep state0
                  (fn [status1 state1 _ error1]
                    (if (ok? status1)
                      (call p state1
                            (fn [status2 state2 value2 error2]
                              (if (ok? status2)
                                (if (same-state? state2 state0)
                                  (throw (infinite-loop-exception sym p state2))
                                  (step reply (rf acc value2) state2 (merge-errors error2 state2 error1 state1)))
                                (if (and sep-end-ok? (error? status2) (same-state? state2 state1))
                                  (reply :ok state2 (rf acc) (error/merge error2 (merge-errors error1 state1 error0 state0)))
                                  (reply status2 state2 nil (merge-errors error2 state2 error1 state1 error0 state0))))))
                      (if (fatal? status1)
                        (reply :fatal state1 nil (merge-errors error1 state1 error0 state0))
                        (reply :ok state1 (rf acc) (merge-errors error1 state1 error0 state0)))))))]
    (fn [state reply]
      (let [acc (rf)]
        (call p state (fn [status1 state1 value1 error1]
                        (if (ok? status1)
                          (step reply (rf acc value1) state1 error1)
                          (if (and empty-ok? (error? status1) (same-state? state1 state))
                            (reply :ok state1 (rf acc) error1)
                            (reply status1 state1 value1 error1)))))))))

(defn reduce-till
  [sym p endp rf empty-ok? include-end?]
  (letfn [(step [reply acc state0 error0]
            (call endp state0
                  (fn [end-status end-state end-value end-error]
                    (if (and (error? end-status) (same-state? end-state state0))
                      (call p state0
                            (fn [status1 state1 value1 error1]
                              (if (ok? status1)
                                (if (same-state? state1 state0)
                                  (throw (infinite-loop-exception sym p state1))
                                  (step reply (rf acc value1) state1 error1))
                                (reply status1 state1 value1 (if (same-state? state1 state0)
                                                               (error/merge error1 (error/merge end-error error0))
                                                               error1)))))
                      (if (ok? end-status)
                        (reply :ok end-state
                               (rf (if include-end? (rf acc end-value) acc))
                               (merge-errors end-error end-state error0 state0))
                        (reply end-status end-state nil (merge-errors end-error end-state error0 state0)))))))]
    (if empty-ok?
      (fn [state reply]
        (step reply (rf) state nil))
      (fn [state reply]
        (call p state (fn [status1 state1 value1 error1]
                        (if (ok? status1)
                          (if (same-state? state1 state)
                            (throw (infinite-loop-exception sym p state1))
                            (step reply (rf (rf) value1) state1 error1))
                          (reply status1 state1 value1 error1))))))))

(defn pforce
  "Forces a possibly delayed parser `dp`.  Implementation detail of
   the `lazy` parser."
  [dp]
  (fn [state reply]
    (let [p (force dp)]
      (thunk (p state reply)))))
