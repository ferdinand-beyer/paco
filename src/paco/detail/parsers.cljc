(ns paco.detail.parsers
  "Advanced low-level parsers, used by higher level ones."
  (:require [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.scanner :as scanner])
  #?(:cljs (:require-macros [paco.detail.parsers])))

(defn pforce
  "Forces a delayed parser `dp`."
  [dp]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [p (force dp)]
        (parser/apply p scanner reply)))
    (children [_] [(force dp)])))

(defn pseq
  ([ps]
   (pseq ps rfs/rvec))
  ([ps rf]
   (let [ps (vec ps)]
     (reify parser/IParser
       (apply [_ scanner reply]
         (loop [i 0
                result (rf)
                modcount (scanner/modcount scanner)
                error nil]
           (if (< i (count ps))
             (let [p (get ps i)
                   reply (parser/apply p scanner reply)
                   modcount* (scanner/modcount scanner)]
               (if (reply/ok? reply)
                 (recur (unchecked-inc i)
                        (rf result (reply/value reply))
                        modcount*
                        (if (= modcount modcount*)
                          (error/merge error (reply/error reply))
                          (reply/error reply)))
                 (if (= modcount modcount*)
                   (reply/with-error reply (error/merge error (reply/error reply)))
                   reply)))
             (reply/with-value reply (rf result)))))
       (children [_] ps)))))

(defn- emit-apply-seq
  "Emits code that will apply the parsers `ps` in sequence
   and evaluate `body` with the binding forms `bfs` bound
   to the parser return values."
  ([ps scanner reply bfs body]
   (emit-apply-seq ps scanner reply bfs body -1 [] nil))
  ([ps scanner reply bfs body modcount values error]
   {:pre [(seq ps) (symbol? scanner) (symbol? reply)]}
   (let [depth     (inc (count values))
         modcount* (symbol (str "modcount" depth))
         value*    (symbol (str "value" depth))
         error*    (symbol (str "error" depth))]
     `(let [~reply (parser/apply ~(first ps) ~scanner ~reply)]
        (if (reply/ok? ~reply)
          ~(if-some [next-ps (next ps)]
             ;; recur to next parsers
             `(let [~value* (reply/value ~reply)
                    ~error* (reply/error ~reply)
                    ~modcount* (scanner/modcount ~scanner)]
                ~(emit-apply-seq next-ps scanner reply bfs body
                                 modcount* (conj values value*)
                                 (if error
                                   `(if (= ~modcount ~modcount*)
                                      (error/merge ~error ~error*)
                                      ~error*)
                                   error*)))
             ;; this was the last parser
             (let [bindings (interleave bfs (conj values `(reply/value ~reply)))]
               (if error
                 `(let [result# (let [~@bindings] ~@body)
                        ~error* (reply/error ~reply)]
                    (if (= ~modcount (scanner/modcount ~scanner))
                      (reply/ok ~reply result# (error/merge ~error ~error*))
                      (reply/with-value ~reply result#)))
                 `(reply/with-value ~reply (let [~@bindings] ~@body)))))
          ;; parser failed
          ~(if error
             `(let [~error* (reply/error ~reply)]
                (if (= ~modcount (scanner/modcount ~scanner))
                  (reply/with-error ~reply (error/merge ~error ~error*))
                  ~reply))
             reply))))))

(defn- emit-with-seq [bindings body]
  {:pre [(vector? bindings) (even? (count bindings))]}
  (let [pairs (partition 2 bindings)
        bfs   (map first pairs)
        ps    (map second pairs)]
    (assert (every? symbol? ps))
    `(reify parser/IParser
       (~'apply [~'_ ~'scanner ~'reply]
         ~(emit-apply-seq ps 'scanner 'reply bfs body))
       (~'children [~'_] [~@ps]))))

;; Like pseq, but unrolled
(defmacro with-seq
  "Creates a parser that applies parsers in sequence and transforms
   the return values."
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  (emit-with-seq bindings body))

(defn- infinite-loop-exception [sym p scanner]
  (ex-info (str "Parser supplied to '" sym "' succeeded without changing the parser state")
           {:type ::infinite-loop
            :parser p
            :combinator sym
            :position (scanner/position scanner)}))

(defn- apply-many [sym p rf scanner reply result]
  (loop [result result
         modcount (scanner/modcount scanner)
         error nil]
    (let [reply     (parser/apply p scanner reply)
          modcount* (scanner/modcount scanner)]
      (if (reply/ok? reply)
        (if (= modcount modcount*)
          (throw (infinite-loop-exception sym p scanner))
          (recur (rf result (reply/value reply)) modcount* (reply/error reply)))
        (if (= modcount modcount*)
          (reply/ok reply result (error/merge error (reply/error reply)))
          reply)))))

(defn- apply-times [sym p rf n scanner reply result]
  (loop [i 0
         result result
         modcount (scanner/modcount scanner)
         error nil]
    (if (< i n)
      (let [reply     (parser/apply p scanner reply)
            modcount* (scanner/modcount scanner)]
        (if (reply/ok? reply)
          (if (= modcount modcount*)
            (throw (infinite-loop-exception sym p scanner))
            (recur (unchecked-inc i)
                   (rf result (reply/value reply))
                   modcount*
                   (reply/error reply)))
          (if (= modcount modcount*)
            (reply/with-error reply (error/merge error (reply/error reply)))
            reply)))
      (reply/ok reply result error))))

(defn- apply-max [sym p rf max scanner reply result]
  (loop [i 0
         result result
         modcount (scanner/modcount scanner)
         error nil]
    (if (< i max)
      (let [reply     (parser/apply p scanner reply)
            modcount* (scanner/modcount scanner)]
        (if (reply/ok? reply)
          (if (= modcount modcount*)
            (throw (infinite-loop-exception sym p scanner))
            (recur (unchecked-inc i)
                   (rf result (reply/value reply))
                   modcount*
                   (reply/error reply)))
          (if (= modcount modcount*)
            (reply/ok reply result (error/merge error (reply/error reply)))
            reply)))
      (reply/ok reply result error))))

(defn repeat-many [sym p rf]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [reply (apply-many sym p rf scanner reply (rf))]
        (cond-> reply
          (reply/ok? reply) (reply/with-value (rf (reply/value reply))))))
    (children [_] [p])))

(defn repeat-times [sym p rf n]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [reply (apply-times sym p rf n scanner reply (rf))]
        (cond-> reply
          (reply/ok? reply) (reply/with-value (rf (reply/value reply))))))
    (children [_] [p])))

(defn repeat-min [sym p rf min]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [reply (apply-times sym p rf min scanner reply (rf))]
        (if (reply/ok? reply)
          (let [reply (apply-many sym p rf scanner reply (reply/value reply))]
            (cond-> reply
              (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))
          reply)))
    (children [_] [p])))

(defn repeat-max [sym p rf max]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [reply (apply-max sym p rf max scanner reply (rf))]
        (cond-> reply
          (reply/ok? reply) (reply/with-value (rf (reply/value reply))))))
    (children [_] [p])))

(defn repeat-min-max [sym p rf min max]
  (let [max* (- max min)]
    (reify parser/IParser
      (apply [_ scanner reply]
        (let [reply (apply-times sym p rf min scanner reply (rf))]
          (if (reply/ok? reply)
            (let [reply (apply-max sym p rf max* scanner reply (reply/value reply))]
              (cond-> reply
                (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))
            reply)))
      (children [_] [p]))))
