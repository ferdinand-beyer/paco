(ns paco.detail.parsers
  "Advanced low-level parsers, used by higher level ones."
  (:refer-clojure :exclude [reduce])
  (:require [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.reply :as reply]
            [paco.detail.scanner :as scanner])
  #?(:cljs (:require-macros [paco.detail.parsers])))

(defn reduce [ps rf]
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
      (children [_] ps))))

(defn- emit-apply-seq
  "Emits code that will apply the parsers `ps` in sequence
   and evaluate `body` with the `binding-forms` bound to the
   parser return values."
  ([ps scanner reply binding-forms body]
   (emit-apply-seq ps scanner reply binding-forms body -1 [] nil))
  ([ps scanner reply binding-forms body modcount values error]
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
                ~(emit-apply-seq next-ps scanner reply binding-forms body
                                 modcount*
                                 (conj values value*)
                                 (if error
                                   `(if (= ~modcount ~modcount*)
                                      (error/merge ~error ~error*)
                                      ~error*)
                                   error*)))
             ;; this was the last parser
             (let [bindings (interleave binding-forms (conj values `(reply/value ~reply)))]
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

;;---------------------------------------------------------
;; Repeat parsers

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

;; Similar to fparsec's Inline.Many:
;; - stateFromFirstElement ~= (rf)
;; - foldState = (rf result value)
;; - resultFromState = (rf result)
;; - elementParser = p
;; - firstElementParser n/a
;; - resultForEmptySequence: n/a
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

;;---------------------------------------------------------
;; Separated-by

;; Similar to fparsec's Inline.SepBy
;; ? Add include-sep? option
;; ? Support "must end", e.g. 3 sep-end-modes: reject (default), accept, require
(defn sep-by
  [sym p psep rf empty-ok? sep-end-ok?]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [result (rf)
            modcount (scanner/modcount scanner)
            reply (parser/apply p scanner reply)]
        (if (reply/ok? reply)
          ;; Got first `p`, now match `(sep p)*`.
          (loop [result (rf result (reply/value reply))
                 modcount (scanner/modcount scanner)
                 error (reply/error reply)]
            ;; Apply `psep`.
            (let [reply (parser/apply psep scanner reply)
                  modcount-sep (scanner/modcount scanner)
                  error-sep (reply/error reply)]
              (if (reply/ok? reply)
                ;; Got `sep`, now match `p`.
                (let [reply (parser/apply p scanner reply)
                      modcount* (scanner/modcount scanner)
                      error* (reply/error reply)]
                  (if (reply/ok? reply)
                    (if (= modcount modcount*)
                      ;; At least one of sep or p need to advance the scanner.
                      (throw (infinite-loop-exception sym p scanner))
                      (recur (rf result (reply/value reply))
                             modcount*
                             (if (= modcount-sep modcount*)
                               (error/merge error-sep error*)
                               error*)))
                    ;; `p` failed.
                    (if (and sep-end-ok? (= modcount-sep modcount*))
                      (reply/ok reply
                                (rf result)
                                (error/merge (if (= modcount modcount-sep)
                                               (error/merge error error-sep)
                                               error-sep)
                                             error*))
                      (reply/with-error reply (if (= modcount-sep modcount*)
                                                (error/merge (if (= modcount modcount-sep)
                                                               (error/merge error error-sep)
                                                               error-sep)
                                                             error*)
                                                error*)))))
                ;; `sep` failed.
                (reply/ok reply
                          (rf result)
                          (if (= modcount modcount-sep)
                            (error/merge error error-sep)
                            error-sep)))))
          ;; Nothing matched.
          (if (and empty-ok? (= modcount (scanner/modcount scanner)))
            (reply/ok reply (rf result) (reply/error reply))
            reply))))
    (children [_] [p psep])))

;;---------------------------------------------------------
;; Until

;; Similar to fparsec's Inline.ManyTill
(defn until
  [sym p pend rf empty-ok? include-end?]
  (letfn [(apply-until [scanner reply result modcount error]
            (let [reply (parser/apply pend scanner reply)
                  modcount-end (scanner/modcount scanner)
                  error-end (reply/error reply)]
              (if (reply/ok? reply)
                ;; Matched `pend` => end iteration.
                (reply/ok reply
                          (rf (cond-> result
                                include-end? (rf (reply/value reply))))
                          (if (= modcount modcount-end)
                            (error/merge error error-end)
                            error-end))
                (if (= modcount modcount-end)
                  ;; `pend` failed without consuming => match `p`.
                  (let [reply (parser/apply p scanner reply)
                        modcount* (scanner/modcount scanner)
                        error* (reply/error reply)]
                    (if (reply/ok? reply)
                      ;; `p` matched => recur
                      (if (= modcount modcount*)
                        (throw (infinite-loop-exception sym p scanner))
                        (recur scanner
                               reply
                               (rf result (reply/value reply))
                               modcount*
                               error*))
                      ;; `p` failed => return error
                      (reply/with-error reply (if (= modcount modcount*)
                                                (error/merge (error/merge error error-end) error*)
                                                error*))))
                  ;; `pend` failed and consumed.
                  reply))))]
    (if empty-ok?
      ;; Repeatedly check for `pend` or apply `p`.
      (reify parser/IParser
        (apply [_ scanner reply]
          (apply-until scanner reply (rf) (scanner/modcount scanner) nil))
        (children [_] [p pend]))
      ;; Require at least one `p`, then continue as above.
      (reify parser/IParser
        (apply [_ scanner reply]
          (let [result (rf)
                modcount (scanner/modcount scanner)
                reply (parser/apply p scanner reply)
                modcount* (scanner/modcount scanner)]
            (if (reply/ok? reply)
              (if (= modcount modcount*)
                (throw (infinite-loop-exception sym p scanner))
                (apply-until scanner reply
                             (rf result (reply/value reply))
                             modcount*
                             (reply/error reply)))
              reply)))
        (children [_] [p pend])))))
