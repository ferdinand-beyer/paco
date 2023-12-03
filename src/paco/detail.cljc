(ns paco.detail
  (:require [paco.error :as error])
  #?(:cljs (:require-macros [paco.detail :refer [same-state? thunk]])))

(def ^:const ok ::ok)
(def ^:const fail ::fail)
(def ^:const fatal ::fatal)

(defn ok? [status]
  (#?(:clj identical?, :cljs keyword-identical?) ok status))

(defn fail? [status]
  (not (#?(:clj identical?, :cljs keyword-identical?) ok status)))

(defn fatal? [status]
  (#?(:clj identical?, :cljs keyword-identical?) fatal status))

(defmacro same-state? [state other]
  `(identical? ~state ~other))

(defn pass-error [state error prev-state prev-error]
  (if (same-state? state prev-state)
    error
    (error/merge prev-error error)))

(defmacro thunk [& body]
  `(fn [] ~@body))

(defn run-parser [p state reply]
  (trampoline #(p state reply)))

;;---------------------------------------------------------
;; Reducing functions

(defn ignore
  "Ignores all args and returns `nil`.  Useful for skip parsers."
  [& _]
  nil)

(defn vector-rf
  "Reducing function that collects input in a vector.
   Like `conj!`, but completes with a persistent collection."
  ([] (transient []))
  ([coll] (persistent! coll))
  ([coll x] (conj! coll x)))

(defn then-rf
  ([] nil)
  ([result] result)
  ([_ input] input))

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
     (if (seqexp-tag (meta x))
       (reduce conj! xs x)
       (conj! xs x)))))

;;---------------------------------------------------------
;; Advanced parsers

;; alternate names: >>reduce, preduce, reduce-seq
(defn reduce-sequence
  "Creates a parser that runs `ps` in sequence, reducing their return
   values with the reducing function `rf`."
  [rf ps]
  (letfn [(step-fn [p step]
            (fn [reply state acc error]
              (letfn [(reply1 [status state1 value error1]
                        (if (ok? status)
                          (step reply state1 (rf acc value) (pass-error state1 error1 state error))
                          (reply status state1 value (pass-error state1 error1 state error))))]
                (thunk (p state reply1)))))
          (complete [reply state acc error]
            (reply ok state (rf acc) error))
          (compile [ps]
            (if-let [p (first ps)]
              (step-fn p (compile (next ps)))
              complete))]
    (let [step (compile ps)]
      (fn [state reply]
        (step reply state (rf) nil)))))

(defn- state-unchanged-exception [sym p]
  (ex-info (str "Parser supplied to '" sym "' succeeded without changing the parser state")
           {:type ::state-unchanged
            :parser sym
            :arg p}))

(defn reduce-repeat
  ([sym p rf min]
   (reduce-repeat sym p rf min nil))
  ([sym p rf min max]
   {:pre [(nat-int? min) (or (nil? max) (<= min max))]}
   (letfn [(step [reply acc n state1 error1]
             (letfn [(step-reply [status state2 value error2]
                       (if (ok? status)
                         (if (same-state? state1 state2)
                           (throw (state-unchanged-exception sym p))
                           (let [acc (rf acc value)
                                 n   (inc n)]
                             (if (or (nil? max) (< n max))
                               (step reply acc n state2 error2)
                               (reply ok state2 (rf acc) error2))))
                         (let [error (if (and error1 (identical? state1 state2))
                                       (error/merge error1 error2)
                                       error2)]
                           (if (< n min)
                             (reply fail state2 nil error)
                             (reply ok state2 (rf acc) error)))))]
               (thunk (p state1 step-reply))))]
     (if (and max (zero? max))
       (fn [state reply]
         (reply ok state (rf (rf)) nil))
       (fn [state reply]
         (step reply (rf) 0 state nil))))))
