(ns paco.detail.parsers
  "Advanced low-level parsers, used by higher level ones."
  (:require [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.reply :as reply]
            [paco.detail.scanner :as scanner])
  #?(:cljs (:require-macros [paco.detail.parsers :refer [with-seq]])))

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

(defmacro with-seq
  "Creates a parser that applies parsers in sequence and transforms
   the return values."
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  (emit-with-seq bindings body))

;; TODO: Move :)
(defn pipe
  ([p f]
   (with-seq [x p] (f x)))
  ([p1 p2 f]
   (with-seq [x p1, y p2] (f x y)))
  ([p1 p2 p3 f]
   (with-seq [x p1, y p2, z p3] (f x y z)))
  #_([p1 p2 p3 p4 & more]
     (let [ps (list* p1 p2 p3 p4 (butlast more))
           f  (last more)]
       (reduce-seq (completing conj #(apply f %)) ps))))

(comment
  (emit-with-seq '[x p] '(f x))
  (emit-with-seq '[x p, y q] '(f x y))

  ;;
  )
