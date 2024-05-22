(ns paco.detail.parser
  (:refer-clojure :exclude [apply]))

;;? Add a `name` for inspection?
;;? Add `optimize` to optimize the parser graph?
(defprotocol IParser
  (apply [parser source reply])
  (children [parser]))

(extend-protocol IParser
  #?(:clj  clojure.lang.Fn
     :cljs function)
  (apply [f source reply]
    (f source reply))
  (children [_] nil))
