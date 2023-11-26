(ns comparse.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [comparse.state :as state]))

(deftest string-stream-navigation
  (let [s (state/string-stream "example")]
    (is (zero? (state/index s)))
    (is (not (state/at-end? s)))
    (is (= \e (state/peek s)))
    (is (= "exam" (state/peek-str s 4)))

    (testing "skip one char"
      (let [s (state/skip s 1)]
        (is (= 1 (state/index s)))
        (is (not (state/at-end? s)))
        (is (= \x (state/peek s)))
        (is (= "xamp" (state/peek-str s 4)))
        (testing "skip another"
          (let [s (state/skip s 1)]
            (is (= 2 (state/index s)))
            (is (not (state/at-end? s)))
            (is (= \a (state/peek s)))
            (is (= "ampl" (state/peek-str s 4)))))))

    (testing "skip multiple chars"
      (let [s (state/skip s 4)]
        (is (= 4 (state/index s)))
        (is (not (state/at-end? s)))
        (is (= \p (state/peek s)))
        (is (= "ple" (state/peek-str s 4)))))

    (testing "skip to the end"
      (let [s (state/skip s 7)]
        (is (= 7 (state/index s)))
        (is (state/at-end? s))
        (is (nil? (state/peek s)))
        (is (nil? (state/peek-str s 4)))
        (testing "skip further"
          (let [s (state/skip s 1)]
            (is (= 7 (state/index s)))
            (is (state/at-end? s))
            (is (nil? (state/peek s)))
            (is (nil? (state/peek-str s 4)))))))

    (testing "skip past the end"
      (let [s (state/skip s 42)]
        (is (= 7 (state/index s)))
        (is (state/at-end? s))
        (is (nil? (state/peek s)))
        (is (nil? (state/peek-str s 4)))))))

(deftest string-stream-matching
  (let [s (state/string-stream "example")]
    (testing "stream at the start position"
      (is (state/matches-str? s ""))
      (is (state/matches-str? s "e"))
      (is (state/matches-str? s "exam"))
      (is (state/matches-str? s "example"))
      (is (not (state/matches-str? s "examples")))
      (testing "ignoring case"
        (is (state/matches-str-ic? s "EXAM"))
        (is (not (state/matches-str? s "EXAMPLES")))))

    (testing "stream after skipping"
      (let [s (state/skip s (count "exam"))]
        (is (state/matches-str? s ""))
        (is (state/matches-str? s "p"))
        (is (state/matches-str? s "ple"))
        (is (not (state/matches-str? s "ples")))
        (testing "ignoring case"
          (is (state/matches-str-ic? s "PL"))
          (is (not (state/matches-str? s "PLES"))))))

    (testing "stream after skipping to the end"
      (let [s (state/skip s (count "example"))]
        (is (state/matches-str? s ""))
        (is (not (state/matches-str? s "e")))
        (testing "ignoring case"
          (is (state/matches-str-ic? s ""))
          (is (not (state/matches-str? s "E"))))))))
