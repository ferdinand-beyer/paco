(ns paco.util.platform
  (:refer-clojure :exclude [case])
  #?(:cljs (:require-macros [paco.util.platform])))

(defmacro case
  [& {:keys [clj cljs cljd bb default]}]
  #_{:clj-kondo/ignore [:cond-else :unreachable-code]}
  (cond
    #?(:bb true, :default false) (or bb clj default)
    (contains? &env '&env) `(cond
                              (:ns ~'&env) ~(or cljs default)
                              (:nses ~'&env) ~(or cljd default)
                              :else ~(or clj default))
    #?(:clj (:ns &env), :cljs true, :default false) (or cljs default)
    #?(:clj (:nses &env), :cljd true, :default false) (or cljd default)
    :else (or clj default)))
