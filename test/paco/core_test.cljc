(ns paco.core-test
  (:require [clojure.test :refer [deftest is]]
            [paco.core :as p]))

(deftest return
  (is (= :a (p/parse (p/return :a) ""))))
