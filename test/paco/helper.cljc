(ns paco.helper
  (:require [paco.core :as p]
            [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.reply :as reply]
            [paco.detail.scanner :as scanner]))

(defn run
  ([p]
   (run p ""))
  ([p input]
   (let [scanner  (scanner/of input)
         modcount (scanner/modcount scanner)
         reply    (parser/apply p scanner (reply/mutable-reply))]
     {:ok?      (reply/ok? reply)
      :fail?    (not (reply/ok? reply))
      :value    (reply/value reply)
      :error    (reply/error reply)
      :messages (error/message-set (reply/error reply))
      :index    (scanner/index scanner)
      :position (scanner/position scanner)
      :changed? (not= modcount (scanner/modcount scanner))})))

(def any p/any-token)
