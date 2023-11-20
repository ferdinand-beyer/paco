(ns comparse.reply
  (:require [comparse.protocols :as protocols]))

(defprotocol IReply
  (state [reply])
  (errors [reply])
  (then [reply f])
  (else [reply f]))

(deftype Success [state value errors]
  IReply
  (state [_] state)
  (errors [_] errors)
  (then [_ f] (f state value))
  (else [this _f] this))

(deftype Failure [state errors]
  IReply
  (state [_] state)
  (errors [_] errors)
  (then [this _f] this)
  (else [_ f] (f state errors)))

(defn success
  ([state value]
   (success state value nil))
  ([state value errors]
   (Success. state value errors)))

(defn failure
  ([state]
   (failure state nil))
  ([state errors]
   (Failure. state errors)))

(defn success? [reply]
  (instance? Success reply))

(defn failure? [reply]
  (instance? Failure reply))

(defn result [reply]
  (when (success? reply)
    (.-value ^Success reply)))
