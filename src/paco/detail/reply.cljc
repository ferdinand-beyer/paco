(ns paco.detail.reply)

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
