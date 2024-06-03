(ns paco.detail.parsers
  "Advanced low-level parsers, used by higher level ones."
  (:refer-clojure :exclude [sequence])
  (:require [paco.detail.error :as error]
            [paco.detail.reply :as reply]
            [paco.detail.source :as source])
  #?(:cljs (:require-macros [paco.detail.parsers])))

(defn sequence
  "Applies the `parsers` sequentially and collects their return values
   using the reducing function `rf`."
  [rf parsers]
  (fn [source reply]
    (loop [ps (seq parsers)
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

(defn- emit-with-seq
  "Emits code for the `with-seq` macro."
  [bindings body]
  {:pre [(vector? bindings)
         (seq bindings)
         (even? (count bindings))]}
  (let [source (gensym "source__")
        reply  (gensym "reply__")
        emit   (fn emit [[binding-form parser & more-bindings] modcount-before error-before]
                 `(let [~reply (~parser ~source ~reply)]
                    (if (reply/ok? ~reply)
                      (let [~binding-form (reply/value ~reply)]
                        ~(if (seq more-bindings)
                           ;; recur to next parsers
                           (let [modcount (gensym "modcount__")
                                 error    (gensym "error__")]
                             `(let [~modcount (source/modcount ~source)
                                    ~error    (reply/error ~reply)]
                                ~(emit more-bindings
                                       modcount
                                       (if error-before
                                         `(if (= ~modcount-before ~modcount)
                                            (error/merge ~error-before ~error)
                                            ~error)
                                         error))))
                           ;; this was the last parser
                           (if error-before
                             `(let [value# (do ~@body)]
                                (if (= ~modcount-before (source/modcount ~source))
                                  (reply/ok ~reply value# (error/merge ~error-before (reply/error ~reply)))
                                  (reply/with-value ~reply value#)))
                             `(reply/with-value ~reply (do ~@body)))))
                      ;; parser failed
                      ~(if error-before
                         `(let [error# (reply/error ~reply)]
                            (if (= ~modcount-before (source/modcount ~source))
                              (reply/with-error ~reply (error/merge ~error-before error#))
                              ~reply))
                         reply))))]
    (list `fn [source reply] (emit bindings nil nil))))

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

(defn- complete [rf reply]
  (cond-> reply
    (reply/ok? reply) (reply/update-value rf)))

;; Similar to fparsec's Inline.Many:
;; - stateFromFirstElement ~= (rf)
;; - foldState = (rf result value)
;; - resultFromState = (rf result)
;; - elementParser = p
;; - firstElementParser n/a
;; - resultForEmptySequence: n/a
(defn repeat-many [sym p rf]
  (fn [source reply]
    (complete rf (apply-many sym p rf source reply (rf)))))

(defn repeat-times [sym p rf n]
  (fn [source reply]
    (complete rf (apply-times sym p rf n source reply (rf)))))

(defn repeat-min [sym p rf min]
  (fn [source reply]
    (let [reply (apply-times sym p rf min source reply (rf))]
      (if (reply/ok? reply)
        (complete rf (apply-many sym p rf source reply (reply/value reply)))
        reply))))

(defn repeat-max [sym p rf max]
  (fn [source reply]
    (complete rf (apply-max sym p rf max source reply (rf)))))

(defn repeat-min-max [sym p rf min max]
  (let [max* (- max min)]
    (fn [source reply]
      (let [reply (apply-times sym p rf min source reply (rf))]
        (if (reply/ok? reply)
          (complete rf (apply-max sym p rf max* source reply (reply/value reply)))
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
