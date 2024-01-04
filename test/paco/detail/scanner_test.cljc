(ns paco.detail.scanner-test
  (:require [clojure.test :refer [deftest is testing]]
            [paco.detail.position :as pos]
            [paco.detail.scanner :as scanner]))

(deftest string-navigation-test
  (let [s (scanner/of "example")]
    (is (zero? (scanner/index s)))
    (is (not (scanner/end? s)))
    (is (= \e (scanner/peek s)))
    (is (= "exam" (scanner/peek-str s 4))))

  (testing "skip one char"
    (let [s (scanner/of "example")]
      (scanner/untracked-skip! s 1)
      (is (= 1 (scanner/index s)))
      (is (not (scanner/end? s)))
      (is (= \x (scanner/peek s)))
      (is (= "xamp" (scanner/peek-str s 4)))
      (testing "skip another"
        (scanner/untracked-skip! s 1)
        (is (= 2 (scanner/index s)))
        (is (not (scanner/end? s)))
        (is (= \a (scanner/peek s)))
        (is (= "ampl" (scanner/peek-str s 4))))))

  (testing "skip multiple chars"
    (let [s (scanner/of "example")]
      (scanner/untracked-skip! s 4)
      (is (= 4 (scanner/index s)))
      (is (not (scanner/end? s)))
      (is (= \p (scanner/peek s)))
      (is (= "ple" (scanner/peek-str s 4)))))

  (testing "skip to the end"
    (let [s (scanner/of "example")]
      (scanner/untracked-skip! s 7)
      (is (= 7 (scanner/index s)))
      (is (scanner/end? s))
      (is (nil? (scanner/peek s)))
      (is (nil? (scanner/peek-str s 4)))
      (testing "skip further"
        (scanner/untracked-skip! s 1)
        (is (= 7 (scanner/index s)))
        (is (scanner/end? s))
        (is (nil? (scanner/peek s)))
        (is (nil? (scanner/peek-str s 4))))))

  (testing "skip past the end"
    (let [s (scanner/of "example")]
      (scanner/untracked-skip! s 42)
      (is (= 7 (scanner/index s)))
      (is (scanner/end? s))
      (is (nil? (scanner/peek s)))
      (is (nil? (scanner/peek-str s 4))))))

(deftest string-matching-test
  (testing "at the start position"
    (let [s (scanner/of "example")]
      (is (scanner/matches-str? s ""))
      (is (scanner/matches-str? s "e"))
      (is (scanner/matches-str? s "exam"))
      (is (scanner/matches-str? s "example"))
      (is (not (scanner/matches-str? s "examples")))
      (testing "ignoring case"
        (is (scanner/matches-str-ci? s "EXAM"))
        (is (not (scanner/matches-str? s "EXAMPLES"))))))

  (testing "after skipping"
    (let [s (scanner/of "example")]
      (scanner/skip! s (count "exam"))
      (is (scanner/matches-str? s ""))
      (is (scanner/matches-str? s "p"))
      (is (scanner/matches-str? s "ple"))
      (is (not (scanner/matches-str? s "ples")))
      (testing "ignoring case"
        (is (scanner/matches-str-ci? s "PL"))
        (is (not (scanner/matches-str? s "PLES"))))))

  (testing "after skipping to the end"
    (let [s (scanner/of "example")]
      (scanner/skip! s (count "example"))
      (is (scanner/matches-str? s ""))
      (is (not (scanner/matches-str? s "e")))
      (testing "ignoring case"
        (is (scanner/matches-str-ci? s ""))
        (is (not (scanner/matches-str? s "E")))))))

(deftest re-matching-test
  (testing "at start of input"
    (let [s (scanner/of "example")]
      (is (nil? (scanner/re-match s #"foo")))
      (is (nil? (scanner/re-match s #"x")))
      (is (some? (scanner/re-match s #"ex")))
      (is (some? (scanner/re-match s #"^ex")))
      (is (= "ex" (scanner/re-groups s #"ex")))
      (is (= ["exa" "a"] (scanner/re-groups s #"ex(a)")))))

  (testing "after skipping"
    (let [s (scanner/of "example")]
      (scanner/skip! s 4)
      (is (nil? (scanner/re-match s #"ex")))
      (is (some? (scanner/re-match s #"p")))
      (is (nil? (scanner/re-match s #"^p")))
      (is (= "ple" (scanner/re-groups s #"ple")))
      (is (= ["ple" "p" "l"] (scanner/re-groups s #"(p)(.)e")))))

  (testing "at the end of input"
    (let [s (scanner/of "example")]
      (scanner/skip! s 7)
      (is (nil? (scanner/re-match s #"e"))))))

(deftest line-tracking-test
  (testing "keeps line unchanged when not at a newline"
    (let [s (scanner/of "example")]
      (scanner/skip! s)
      (is (= 1 (scanner/index s)))
      (let [pos (scanner/position s)]
        (is (= 0 (pos/line-index pos)))
        (is (= 1 (pos/column-index pos))))))

  (testing "tracks Unix-style newlines"
    (let [s (scanner/of "\n")]
      (scanner/skip! s)
      (is (= 1 (scanner/index s)))
      (let [pos (scanner/position s)]
        (is (= 1 (pos/line-index pos)))
        (is (= 0 (pos/column-index pos))))))

  (testing "tracks Mac-style newlines"
    (let [s (scanner/of "\r")]
      (scanner/skip! s)
      (is (= 1 (scanner/index s)))
      (let [pos (scanner/position s)]
        (is (= 1 (pos/line-index pos)))
        (is (= 0 (pos/column-index pos))))))

  (testing "tracks Windows-style newlines"
    (let [s (scanner/of "\r\n")]
      (scanner/skip! s)
      (is (= 1 (scanner/index s)))
      (let [pos (scanner/position s)]
        (is (= 0 (pos/line-index pos)))
        (is (= 1 (pos/column-index pos))))
      (scanner/skip! s)
      (is (= 2 (scanner/index s)))
      (let [pos (scanner/position s)]
        (is (= 1 (pos/line-index pos)))
        (is (= 0 (pos/column-index pos)))))))
