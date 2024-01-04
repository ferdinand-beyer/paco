(ns paco.detail.reply
  (:require [paco.detail.rfs :as rfs]))

(defprotocol IReplyFactory
  (ok [this value] [this value error])
  (fail [this error]))

(defprotocol IReply
  (ok? [this])
  (value [this])
  (error [this])

  (with-ok [this ok?])
  (with-value [this value])
  (with-error [this error]))

(deftype MutableReply #?(:clj  [^:unsynchronized-mutable ok?*
                                ^:unsynchronized-mutable value*
                                ^:unsynchronized-mutable error*]
                         :cljs [^:mutable ok?*
                                ^:mutable value*
                                ^:mutable error*])
  IReply
  (ok? [_] ok?*)
  (value [_] value*)
  (error [_] error*)

  (with-ok [this ok?]
    (set! ok?* ok?)
    this)
  (with-value [this value]
    (set! value* value)
    this)
  (with-error [this error]
    (set! error* error)
    this)

  IReplyFactory
  (ok [this value]
    (set! ok?* true)
    (set! value* value)
    (set! error* nil)
    this)
  (ok [this value error]
    (set! ok?* true)
    (set! value* value)
    (set! error* error)
    this)
  (fail [this error]
    (set! ok?* false)
    (set! value* nil)
    (set! error* error)
    this))

(defn mutable-reply []
  (MutableReply. true nil nil))

(defprotocol ICollector
  (reducing-fn [this])
  (flatten? [this other]))

(defn collector
  ([rf]
   (collector rf nil))
  ([rf flatten?]
   (let [flatten?* (if (ifn? flatten?)
                     flatten?
                     (constantly flatten?))]
     (reify ICollector
       (reducing-fn [_] rf)
       (flatten? [_ other] (flatten?* other))))))

(defprotocol ICollectorReply
  (step [this] "Folds the reply value into the accumulated result.")
  (complete [this] "Completes the reduction.")
  (-collector [this])
  (-nested [this]))

(deftype CollectorReply #?(:clj [collector rf
                                 ^:unsynchronized-mutable reply*
                                 ^:unsynchronized-mutable acc*
                                 ^:unsynchronized-mutable ^long level*]
                           :cljs [collector rf
                                  ^:mutable reply*
                                  ^:mutable acc*
                                  ^:mutable ^number level*])
  IReply
  (ok? [_] (ok? reply*))
  (value [_] (value reply*))
  (error [_] (error reply*))

  (with-ok [this ok?]
    (set! reply* (with-ok reply* ok?))
    this)
  (with-value [this value]
    (set! reply* (with-value reply* value))
    this)
  (with-error [this error]
    (set! reply* (with-error reply* error))
    this)

  IReplyFactory
  (ok [this value]
    (set! reply* (ok reply* value))
    this)
  (ok [this value error]
    (set! reply* (ok reply* value error))
    this)
  (fail [this error]
    (set! reply* (fail reply* error))
    this)

  ICollectorReply
  (step [this]
    (let [v (value reply*)]
      ;; At the end of a nested accumulator, we will be called in the sequence
      ;; (step) (complete) (step), so we need to make sure not to reduce the
      ;; value twice.
      (when-not (= ::complete v)
        (set! acc* (rf acc* v))))
    this)
  (complete [this]
    (if (zero? level*)
      ;; We are the root: Report the accumulated result to the parent reply.
      ;; This object can now be disposed/reused.
      (cond-> reply* (ok? reply*) (with-value (rf acc*)))
      ;; The child reduction is complete.
      (do
        (set! reply* (with-value reply* ::complete))
        (set! level* (unchecked-dec level*))
        this)))
  (-collector [_] collector)
  (-nested [this]
    (set! level* (unchecked-inc level*))
    this))

;; ? Merge this with `collect`?
(defn collector-reply [collector reply]
  (let [rf (reducing-fn collector)]
    (CollectorReply. collector rf reply (rf) 0)))

(defn collect
  "Wraps `reply` in an `ICollectorReply` to reduce subsequent reply values
   into a single value."
  [collector reply]
  (if (and (satisfies? ICollectorReply reply)
           (flatten? (-collector reply) collector))
    (-nested reply)
    (collector-reply collector reply)))

(def nil-collector
  "A collector that discards all values and returns `nil`."
  (collector (constantly nil) (constantly true)))

(defn- vector-rf
  ([]      (transient []))
  ([acc]   (persistent! acc))
  ([acc x] (conj! acc x)))

(def vector-collector
  (collector vector-rf))

(def first-collector
  (reify ICollector
    (reducing-fn [_] rfs/rfirst)
    (flatten? [this other]
      (identical? this other))))

(def last-collector
  (reify ICollector
    (reducing-fn [_] rfs/rlast)
    (flatten? [this other]
      (identical? this other))))

(defn- seqex-rf
  ([]      (transient []))
  ([acc]   (persistent! acc))
  ([acc x] (cond-> acc (some? x) (conj! x))))

(def seqex-collector
  "A collector for sequence expressions, flattening nested seqex operators."
  (reify ICollector
    (reducing-fn [_] seqex-rf)
    (flatten? [this other]
      (identical? this other))))

(def string-collector
  "A collector that builds a string from all return values."
  (collector rfs/string true))

(comment
  (let [root  (mutable-reply)
        outer (collect string-collector root)]
    (with-value outer 1)
    (step outer)
    (with-value outer 2)
    (step outer)
    (let [inner (collect seqex-collector outer)]
      (with-value inner :a)
      (step inner)
      (with-value inner 7)
      (with-value inner (inc (value inner)))
      (step inner)
      (let [inner (collect seqex-collector inner)]
        (with-value inner :x)
        (step inner)
        (with-value inner :y)
        (step inner)
        (complete inner))
      (step inner)
      (with-value inner :b)
      (step inner)
      (complete inner))
    (step outer)
    (complete outer)
    (value root))

  ;;
  )
