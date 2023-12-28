(ns paco.util.platform-test
  (:require [clojure.test :refer [deftest is]]
            [paco.util.platform :as platform])
  #?(:cljs (:require-macros [paco.util.platform-test :refer [test-macro]])))

(defmacro test-macro []
  (platform/case :clj :clj, :cljs :cljs, :bb :bb))

(def platform #?(:bb :bb, :clj :clj, :cljs :cljs))

(deftest case-test
  (is (= platform (platform/case :clj :clj, :cljs :cljs, :bb :bb)))
  (is (= platform (test-macro))))
