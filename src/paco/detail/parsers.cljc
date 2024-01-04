(ns paco.detail.parsers
  "Advanced low-level parsers, used by higher level ones."
  (:require [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.reply :as reply]
            [paco.detail.scanner :as scanner])
  #?(:cljs (:require-macros [paco.detail.parsers])))

(defn pseq
  ([ps]
   (pseq ps reply/vector-collector))
  ([ps collector]
   (let [ps (vec ps)]
     (reify parser/IParser
       (apply [_ scanner reply]
         (loop [i 0
                reply (reply/collect collector reply)
                modcount -1
                error nil]
           (if (< i (count ps))
             (let [p     (get ps i)
                   reply (parser/apply p scanner reply)
                   mcnt  (long (scanner/modcount scanner))]
               (if (reply/ok? reply)
                 (recur (unchecked-inc i)
                        (reply/step reply)
                        mcnt
                        (if (= modcount mcnt)
                          (error/merge error (reply/error reply))
                          (reply/error reply)))
                 (if (= modcount mcnt)
                   (-> reply
                       (reply/with-error (error/merge error (reply/error reply)))
                       reply/complete)
                   (reply/complete reply))))
             (reply/complete reply))))
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