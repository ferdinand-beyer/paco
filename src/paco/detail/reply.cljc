(ns paco.detail.reply)

(defprotocol IReplyFactory
  (ok [reply-factory value] [reply-factory value error]
    "Creates a successful reply with a result value.  Successful replies
     can also carry an error, representing any additional input the parser
     could have consumed.")
  (fail [reply-factory error]
    "Creates an unsuccessful reply with an error."))

(defprotocol IReply
  (ok? [reply]
    "Returns true if this reply is successful.")
  (value [reply]
    "Returns the result value of the reply.")
  (error [reply]
    "Returns the error of the reply.")

  (with-ok [reply ok?]
    "Returns an updated reply with the success flag set to `ok?`.")
  (with-value [reply value]
    "Returns an updated reply with the result value set to `value`.")
  (with-error [reply error]
    "Returns an updated reply with the error set to `error`."))

(defn update-value
  "Updates the result value of `reply` using `f`."
  [reply f]
  (with-value reply (f (value reply))))

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
    (set! ok?* (boolean ok?))
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

(defn mutable-reply
  "Creates a mutable reply object."
  []
  (MutableReply. true nil nil))
