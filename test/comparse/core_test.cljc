(ns comparse.core-test
  (:require [clojure.test :refer [deftest is]]
            [comparse.core :as p]))

(deftest return
  (is (= :a (p/parse (p/return :a) ""))))
