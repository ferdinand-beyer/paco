(ns paco.helper
  (:require [paco.core :as p]
            [paco.detail.error :as error]
            [paco.detail.reply :as reply]
            [paco.detail.source :as source]))

(defn run
  ([p]
   (run p ""))
  ([p input]
   (let [source   (source/of input)
         modcount (source/modcount source)
         reply    (p source (reply/mutable-reply))]
     {:ok?      (reply/ok? reply)
      :fail?    (not (reply/ok? reply))
      :value    (reply/value reply)
      :error    (reply/error reply)
      :messages (error/message-set (reply/error reply))
      :index    (source/index source)
      :position (source/position source)
      :changed? (not= modcount (source/modcount source))})))

(defn reply [p input]
  (p (source/of input) (reply/mutable-reply)))

(defn fails? [p input]
  (not (reply/ok? (reply p input))))

(def any p/any-token)
