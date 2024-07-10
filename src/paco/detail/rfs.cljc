(ns paco.detail.rfs
  "Reducing functions."
  (:refer-clojure :exclude [first last vector])
  #?(:cljs (:import [goog.string StringBuffer])))

(def ignore
  "Ignores all args and returns `nil`.  Useful for skip parsers."
  (constantly nil))

(defn vector
  "Reducing function that collects input in a vector.
   Like `conj!`, but completes with a persistent collection."
  ([] (transient []))
  ([coll] (persistent! coll))
  ([coll x] (conj! coll x)))

(defn first
  "Reducing function that only keeps the first input."
  ([] ::init)
  ([result] (when-not (= ::init result)
              result))
  ([result input] (if (= ::init result)
                    input
                    result)))

(defn last
  "Reducing function that only keeps the last input."
  ([] nil)
  ([result] result)
  ([_ input] input))

(def ^:private ^:const seqex-tag ::seqex)
(def ^:private ^:const seqex-meta {seqex-tag true})

;; "flatseq"?
(defn seqex
  "Reducing function for 'sequence expression' parsers.

   Nested seqex collections are flattened."
  ([] (transient []))
  ([xs]
   (-> xs
       persistent!
       (with-meta seqex-meta)))
  ([xs x]
   (if (nil? x)
     xs
     (if (contains? (meta x) seqex-tag)
       (reduce conj! xs x)
       (conj! xs x)))))

(defn string
  "Reducing function that builds a string."
  ([] #?(:clj  (StringBuilder.)
         :cljs (StringBuffer.)))
  ([sb] (str sb))
  ([sb x]
   (if (nil? x)
     sb
     (if (coll? x)
       (reduce string sb x)
       (.append ^StringBuilder sb x)))))
