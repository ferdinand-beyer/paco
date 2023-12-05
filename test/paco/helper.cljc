(ns paco.helper
  (:require [paco.detail :as detail]
            [paco.error :as error]
            [paco.state :as state]))

(defn- reply-fn [initial-state]
  (fn [status state value error]
    {:status   status
     :ok?      (detail/ok? status)
     :fail?    (detail/fail? status)
     :changed? (not (detail/same-state? state initial-state))
     :state    state
     :value    value
     :error    error
     :messages (error/message-set error)}))

(defn run
  ([p]
   (run p ""))
  ([p input]
   (let [state (state/of input nil)]
     (detail/run-parser p state (reply-fn state)))))

(defn any [state reply]
  (if-let [token (state/peek state)]
    (reply detail/ok (state/skip state 1) token nil)
    (reply detail/error state nil error/unexpected-eof)))
