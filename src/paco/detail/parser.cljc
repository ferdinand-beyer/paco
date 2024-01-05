(ns paco.detail.parser
  (:refer-clojure :exclude [apply]))

;;? Add a `name` for inspection?
;;? Add `optimize` to optimize the parser graph?
(defprotocol IParser
  (apply [this scanner reply])
  (children [this]))

(extend-protocol IParser
  #?(:clj  clojure.lang.Fn
     :cljs function)
  (apply [f scanner reply]
    (f scanner reply))
  (children [_] nil))
