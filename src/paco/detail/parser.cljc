(ns paco.detail.parser
  (:refer-clojure :exclude [apply]))

(defprotocol IParser
  (apply [this scanner reply])
  (children [this]))

(extend-protocol IParser
  #?(:clj  clojure.lang.Fn
     :cljs function)
  (apply [f scanner reply]
    (f scanner reply))
  (children [_] nil))
