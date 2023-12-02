(ns paco.helper
  (:require [paco.error :as error]
            [paco.reply :as reply]
            [paco.state :as state]))

(defn reply [ok? changed? state value error]
  {:ok?      ok?
   :fail?    (not ok?)
   :changed? changed?
   :state    state
   :value    value
   :error    error
   :messages (error/message-set error)})

(def ctx
  (reply/context
    (fn [state value error]
      (reply true false state value error))
    (fn [state value error]
      (reply true true state value error))
    (fn [state error]
      (reply false false state nil error))
    (fn [state error]
      (reply false true state nil error))))

(defn run
  ([p]
   (run p ""))
  ([p input]
   (trampoline #(p (state/of input nil) ctx))))

(defn any [state ctx]
  (if-let [tok (state/peek state)]
    (reply/ok! ctx (state/skip state 1) tok)
    (reply/fail ctx state error/unexpected-eof)))
