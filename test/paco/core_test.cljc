(ns paco.core-test
  (:require [clojure.test :refer [deftest is]]
            [paco.chars :as c]
            [paco.core :as p]
            [paco.error :as error]
            [paco.helper :as helper]))

(deftest pnil-test
  (let [result (helper/run p/pnil)]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (nil? (:value result)))
    (is (nil? (:error result)))))

(deftest fail-test
  (let [result (helper/run (p/fail "boom!"))]
    (is (:fail? result))
    (is (not (:changed? result)))
    (is (nil? (:value result)))
    (is (= (error/message "boom!") (:error result)))))

(deftest return-test
  (let [result (helper/run (p/return 1))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 1 (:value result)))
    (is (nil? (:error result))))

  (let [result (helper/run (p/return (p/return 1) 2))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 2 (:value result))))

  (let [result (helper/run (p/return helper/any 2) "test")]
    (is (:ok? result))
    (is (:changed? result))
    (is (= 2 (:value result))))

  (let [result (helper/run (p/return (p/fail "boom!") 2))]
    (is (:fail? result))
    (is (not (:changed? result)))))

(deftest bind-test
  (let [result (helper/run (p/bind (p/return 1) #(p/return [% 2])))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= [1 2] (:value result))))

  (let [result (helper/run (p/bind helper/any #(p/return [% 2])) "test")]
    (is (:ok? result))
    (is (:changed? result))
    (is (= [\t 2] (:value result))))

  (let [result (helper/run (p/bind p/pnil (constantly helper/any)) "test")]
    (is (:ok? result))
    (is (:changed? result))
    (is (= \t (:value result)))))

(deftest pipe-test
  (let [result (helper/run (p/pipe (p/return 1) inc))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 2 (:value result))))

  (let [result (helper/run (p/pipe (p/return 1) (p/return 2) +))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 3 (:value result))))

  (let [result (helper/run (p/pipe (p/return 1) (p/return 2) (p/return 3) +))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 6 (:value result))))

  (let [result (helper/run (p/pipe (p/return 1) (p/return 2) (p/return 3)
                                   (p/return 4) +))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 10 (:value result))))

  (let [result (helper/run (p/pipe (p/return 1) (p/return 2) (p/return 3)
                                   (p/return 4) (p/return 5) +))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= 15 (:value result))))

  (let [result (helper/run (p/pipe helper/any helper/any helper/any vector) "abc")]
    (is (:ok? result))
    (is (:changed? result))
    (is (= [\a \b \c] (:value result))))

  (let [result (helper/run (p/pipe helper/any helper/any helper/any
                                   helper/any helper/any helper/any
                                   vector) "abcdef")]
    (is (:ok? result))
    (is (:changed? result))
    (is (= [\a \b \c \d \e \f] (:value result))))

  (let [result (helper/run (p/pipe helper/any helper/any helper/any
                                   helper/any helper/any helper/any
                                   (p/fail "boom!") vector) "abcdef")]
    (is (:fail? result))
    (is (:changed? result))
    (is (nil? (:value result)))
    (is (= (error/message "boom!") (:error result)))))

(deftest sequence-test
  (let [result (helper/run (p/sequence [(p/return :begin) (p/return :end)]))]
    (is (:ok? result))
    (is (not (:changed? result)))
    (is (= [:begin :end] (:value result))))

  (let [result (helper/run (p/sequence [(p/return :begin) helper/any (p/return :end)]) "test")]
    (is (:ok? result))
    (is (:changed? result))
    (is (= [:begin \t :end] (:value result))))

  (let [result (helper/run (p/sequence [(p/return :begin) (p/fail "boom!")]))]
    (is (:fail? result))
    (is (not (:changed? result)))
    (is (= (error/message "boom!") (:error result))))

  (let [result (helper/run (p/sequence [helper/any (p/fail "boom!")]) "test")]
    (is (:fail? result))
    (is (:changed? result))
    (is (= (error/message "boom!") (:error result)))))

(deftest group-test
  (is (= [\a \b \c] (p/parse (p/group helper/any helper/any helper/any) "abcdef"))))

(deftest >>-test
  (is (= \c (p/parse (p/>> helper/any helper/any helper/any) "abcdef"))))

(deftest between-test
  (is (= \x (p/parse (-> helper/any (p/between (c/char \() (c/char \)))) "(x)"))))

(deftest attempt-test
  (is (= :foo-fighters (-> (p/alt (p/attempt (p/cat (c/string "foo")
                                                    (c/string "bar")))
                                  (p/return (c/string-i "foo fighters") :foo-fighters))
                           (p/parse "Foo Fighters are a band"))))

  (let [result (helper/run (p/attempt helper/any))]
    (is (:fail? result))
    (is (not (:changed? result)))
    (is (= error/unexpected-eof (:error result))))

  (let [result (helper/run (p/attempt (p/>> helper/any helper/any)) "x")]
    (is (:fail? result))
    (is (not (:changed? result)))
    (is (= ::error/nested (get-in result [:error :type])))))
