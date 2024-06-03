(ns paco.detail.parsers
  "Advanced low-level parsers, used by higher level ones."
  (:refer-clojure :exclude [reduce])
  (:require [paco.detail.error :as error]
            [paco.detail.reply :as reply]
            [paco.detail.source :as source])
  #?(:cljs (:require-macros [paco.detail.parsers])))

(defn reduce
  "Applies the parsers `ps` in sequence, accumulating their return values
   using the reducing function `rf`."
  [ps rf]
  (fn [source reply]
    (loop [ps (seq ps)
           result (rf)
           modcount1 (source/modcount source)
           error1 nil]
      (if-some [p (first ps)]
        (let [reply     (p source reply)
              modcount2 (source/modcount source)]
          (if (reply/ok? reply)
            (recur (next ps)
                   (rf result (reply/value reply))
                   modcount2
                   (if (= modcount1 modcount2)
                     (error/merge error1 (reply/error reply))
                     (reply/error reply)))
            (if (= modcount1 modcount2)
              (reply/with-error reply (error/merge error1 (reply/error reply)))
              reply)))
        (reply/with-value reply (rf result))))))

(defn- emit-apply-seq
  "Emits code that will apply the parsers `ps` in sequence
   and evaluate `body` with the `binding-forms` bound to the
   parser return values."
  ([ps source reply binding-forms body]
   (emit-apply-seq ps source reply binding-forms body -1 [] nil))
  ([ps source reply binding-forms body modcount values error]
   {:pre [(seq ps) (symbol? source) (symbol? reply)]}
   (let [depth     (inc (count values))
         modcount* (symbol (str "modcount" depth))
         value*    (symbol (str "value" depth))
         error*    (symbol (str "error" depth))]
     `(let [~reply (~(first ps) ~source ~reply)]
        (if (reply/ok? ~reply)
          ~(if-some [next-ps (next ps)]
             ;; recur to next parsers
             `(let [~value* (reply/value ~reply)
                    ~error* (reply/error ~reply)
                    ~modcount* (source/modcount ~source)]
                ~(emit-apply-seq next-ps source reply binding-forms body
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
                    (if (= ~modcount (source/modcount ~source))
                      (reply/ok ~reply result# (error/merge ~error ~error*))
                      (reply/with-value ~reply result#)))
                 `(reply/with-value ~reply (let [~@bindings] ~@body)))))
          ;; parser failed
          ~(if error
             `(let [~error* (reply/error ~reply)]
                (if (= ~modcount (source/modcount ~source))
                  (reply/with-error ~reply (error/merge ~error ~error*))
                  ~reply))
             reply))))))

(defn- emit-with-seq [bindings body]
  {:pre [(vector? bindings) (even? (count bindings))]}
  (let [pairs (partition 2 bindings)
        bfs   (map first pairs)
        ps    (map second pairs)]
    (assert (every? symbol? ps))
    (list `fn '[source reply] (emit-apply-seq ps 'source 'reply bfs body))))

(defmacro with-seq
  "Creates a parser that applies parsers in sequence and transforms
   the return values."
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  (emit-with-seq bindings body))

;;---------------------------------------------------------
;; Repeat parsers

(defn- infinite-loop-exception [sym p source]
  (ex-info (str "Parser supplied to '" sym "' succeeded without changing the parser state")
           {:type ::infinite-loop
            :parser p
            :combinator sym
            :position (source/position source)}))

(defn- apply-many [sym p rf source reply result]
  (loop [result result
         modcount (source/modcount source)
         error nil]
    (let [reply     (p source reply)
          modcount* (source/modcount source)]
      (if (reply/ok? reply)
        (if (= modcount modcount*)
          (throw (infinite-loop-exception sym p source))
          (recur (rf result (reply/value reply)) modcount* (reply/error reply)))
        (if (= modcount modcount*)
          (reply/ok reply result (error/merge error (reply/error reply)))
          reply)))))

(defn- apply-times [sym p rf n source reply result]
  (loop [i 0
         result result
         modcount (source/modcount source)
         error nil]
    (if (< i n)
      (let [reply     (p source reply)
            modcount* (source/modcount source)]
        (if (reply/ok? reply)
          (if (= modcount modcount*)
            (throw (infinite-loop-exception sym p source))
            (recur (unchecked-inc i)
                   (rf result (reply/value reply))
                   modcount*
                   (reply/error reply)))
          (if (= modcount modcount*)
            (reply/with-error reply (error/merge error (reply/error reply)))
            reply)))
      (reply/ok reply result error))))

(defn- apply-max [sym p rf max source reply result]
  (loop [i 0
         result result
         modcount (source/modcount source)
         error nil]
    (if (< i max)
      (let [reply     (p source reply)
            modcount* (source/modcount source)]
        (if (reply/ok? reply)
          (if (= modcount modcount*)
            (throw (infinite-loop-exception sym p source))
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
  (fn [source reply]
    (let [reply (apply-many sym p rf source reply (rf))]
      (cond-> reply
        (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))))

(defn repeat-times [sym p rf n]
  (fn [source reply]
    (let [reply (apply-times sym p rf n source reply (rf))]
      (cond-> reply
        (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))))

(defn repeat-min [sym p rf min]
  (fn [source reply]
    (let [reply (apply-times sym p rf min source reply (rf))]
      (if (reply/ok? reply)
        (let [reply (apply-many sym p rf source reply (reply/value reply))]
          (cond-> reply
            (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))
        reply))))

(defn repeat-max [sym p rf max]
  (fn [source reply]
    (let [reply (apply-max sym p rf max source reply (rf))]
      (cond-> reply
        (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))))

(defn repeat-min-max [sym p rf min max]
  (let [max* (- max min)]
    (fn [source reply]
      (let [reply (apply-times sym p rf min source reply (rf))]
        (if (reply/ok? reply)
          (let [reply (apply-max sym p rf max* source reply (reply/value reply))]
            (cond-> reply
              (reply/ok? reply) (reply/with-value (rf (reply/value reply)))))
          reply)))))

;;---------------------------------------------------------
;; Separated-by

;; Similar to fparsec's Inline.SepBy
;; ? Add include-sep? option
;; ? Support "must end", e.g. 3 sep-end-modes: reject (default), accept, require
(defn sep-by
  [sym p psep rf empty-ok? sep-end-ok?]
  (fn [source reply]
    (let [result   (rf)
          modcount (source/modcount source)
          reply    (p source reply)]
      (if (reply/ok? reply)
        ;; Got first `p`, now match `(sep p)*`.
        (loop [result   (rf result (reply/value reply))
               modcount (source/modcount source)
               error    (reply/error reply)]
          ;; Apply `psep`.
          (let [reply        (psep source reply)
                modcount-sep (source/modcount source)
                error-sep    (reply/error reply)]
            (if (reply/ok? reply)
              ;; Got `sep`, now match `p`.
              (let [reply     (p source reply)
                    modcount* (source/modcount source)
                    error*    (reply/error reply)]
                (if (reply/ok? reply)
                  (if (= modcount modcount*)
                    ;; At least one of sep or p needs to advance the source.
                    (throw (infinite-loop-exception sym p source))
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
        (if (and empty-ok? (= modcount (source/modcount source)))
          (reply/ok reply (rf result) (reply/error reply))
          reply)))))

;;---------------------------------------------------------
;; Until

;; Similar to fparsec's Inline.ManyTill
(defn until
  [sym p pend rf empty-ok? include-end?]
  (letfn [(apply-until [source reply result modcount error]
            (let [reply        (pend source reply)
                  modcount-end (source/modcount source)
                  error-end    (reply/error reply)]
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
                  (let [reply     (p source reply)
                        modcount* (source/modcount source)
                        error*    (reply/error reply)]
                    (if (reply/ok? reply)
                      ;; `p` matched => recur
                      (if (= modcount modcount*)
                        (throw (infinite-loop-exception sym p source))
                        (recur source
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
      (fn [source reply]
        (apply-until source reply (rf) (source/modcount source) nil))
      ;; Require at least one `p`, then continue as above.
      (fn [source reply]
        (let [result (rf)
              modcount  (source/modcount source)
              reply     (p source reply)
              modcount* (source/modcount source)]
          (if (reply/ok? reply)
            (if (= modcount modcount*)
              (throw (infinite-loop-exception sym p source))
              (apply-until source reply
                           (rf result (reply/value reply))
                           modcount*
                           (reply/error reply)))
            reply))))))
