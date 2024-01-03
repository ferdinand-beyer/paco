(ns paco.detail.reply)

(defprotocol IReplyFactory
  (ok [this value] [this value error])
  (fail [this error])
  (fatal [this error]))

(defprotocol IReply
  (status [this])
  (value [this])
  (error [this])

  (with-status [this status])
  (with-value [this value])
  (with-error [this error]))

(defn ok? [reply]
  (identical? ::ok (status reply)))

(defn not-ok? [reply]
  (not (identical? ::ok (status reply))))

(defn error? [reply]
  (identical? ::error (status reply)))

(defn fatal? [reply]
  (identical? ::fatal (status reply)))

(defn with-ok [reply]
  (with-status reply ::ok))

(deftype MutableReply #?(:clj  [^:unsynchronized-mutable status*
                                ^:unsynchronized-mutable value*
                                ^:unsynchronized-mutable error*]
                         :cljs [^:mutable status*
                                ^:mutable value*
                                ^:mutable error*])
  IReply
  (status [_] status*)
  (value [_] value*)
  (error [_] error*)

  (with-status [this status]
    (set! status* status)
    this)
  (with-value [this value]
    (set! value* value)
    this)
  (with-error [this error]
    (set! error* error)
    this)

  IReplyFactory
  (ok [this value]
    (set! status* ::ok)
    (set! value* value)
    (set! error* nil)
    this)
  (ok [this value error]
    (set! status* ::ok)
    (set! value* value)
    (set! error* error)
    this)
  (fail [this error]
    (set! status* ::error)
    (set! value* nil)
    (set! error* error)
    this)
  (fatal [this error]
    (set! status* ::fatal)
    (set! value* nil)
    (set! error* error)
    this))
