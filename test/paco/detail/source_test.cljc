(ns paco.detail.source-test
  (:require [clojure.test :refer [deftest is testing]]
            [paco.detail.position :as pos]
            [paco.detail.source :as source]))

(deftest string-navigation-test
  (let [s (source/of "example")]
    (is (zero? (source/index s)))
    (is (not (source/end? s)))
    (is (= \e (source/peek s)))
    (is (= "exam" (source/peek-str s 4))))

  (testing "skip one char"
    (let [s (source/of "example")]
      (source/untracked-skip! s 1)
      (is (= 1 (source/index s)))
      (is (not (source/end? s)))
      (is (= \x (source/peek s)))
      (is (= "xamp" (source/peek-str s 4)))
      (testing "skip another"
        (source/untracked-skip! s 1)
        (is (= 2 (source/index s)))
        (is (not (source/end? s)))
        (is (= \a (source/peek s)))
        (is (= "ampl" (source/peek-str s 4))))))

  (testing "skip multiple chars"
    (let [s (source/of "example")]
      (source/untracked-skip! s 4)
      (is (= 4 (source/index s)))
      (is (not (source/end? s)))
      (is (= \p (source/peek s)))
      (is (= "ple" (source/peek-str s 4)))))

  (testing "skip to the end"
    (let [s (source/of "example")]
      (source/untracked-skip! s 7)
      (is (= 7 (source/index s)))
      (is (source/end? s))
      (is (nil? (source/peek s)))
      (is (nil? (source/peek-str s 4)))
      (testing "skip further"
        (source/untracked-skip! s 1)
        (is (= 7 (source/index s)))
        (is (source/end? s))
        (is (nil? (source/peek s)))
        (is (nil? (source/peek-str s 4))))))

  (testing "skip past the end"
    (let [s (source/of "example")]
      (source/untracked-skip! s 42)
      (is (= 7 (source/index s)))
      (is (source/end? s))
      (is (nil? (source/peek s)))
      (is (nil? (source/peek-str s 4))))))

(deftest string-matching-test
  (testing "at the start position"
    (let [s (source/of "example")]
      (is (source/matches-str? s ""))
      (is (source/matches-str? s "e"))
      (is (source/matches-str? s "exam"))
      (is (source/matches-str? s "example"))
      (is (not (source/matches-str? s "examples")))
      (testing "ignoring case"
        (is (source/matches-str-ci? s "EXAM"))
        (is (not (source/matches-str? s "EXAMPLES"))))))

  (testing "after skipping"
    (let [s (source/of "example")]
      (source/skip! s (count "exam"))
      (is (source/matches-str? s ""))
      (is (source/matches-str? s "p"))
      (is (source/matches-str? s "ple"))
      (is (not (source/matches-str? s "ples")))
      (testing "ignoring case"
        (is (source/matches-str-ci? s "PL"))
        (is (not (source/matches-str? s "PLES"))))))

  (testing "after skipping to the end"
    (let [s (source/of "example")]
      (source/skip! s (count "example"))
      (is (source/matches-str? s ""))
      (is (not (source/matches-str? s "e")))
      (testing "ignoring case"
        (is (source/matches-str-ci? s ""))
        (is (not (source/matches-str? s "E")))))))

(deftest re-matching-test
  (testing "at start of input"
    (let [s (source/of "example")]
      (is (nil? (source/re-match s #"foo")))
      (is (nil? (source/re-match s #"x")))
      (is (some? (source/re-match s #"ex")))
      (is (some? (source/re-match s #"^ex")))
      (is (= "ex" (source/re-groups s #"ex")))
      (is (= ["exa" "a"] (source/re-groups s #"ex(a)")))))

  (testing "after skipping"
    (let [s (source/of "example")]
      (source/skip! s 4)
      (is (nil? (source/re-match s #"ex")))
      (is (some? (source/re-match s #"p")))
      (is (nil? (source/re-match s #"^p")))
      (is (= "ple" (source/re-groups s #"ple")))
      (is (= ["ple" "p" "l"] (source/re-groups s #"(p)(.)e")))))

  (testing "at the end of input"
    (let [s (source/of "example")]
      (source/skip! s 7)
      (is (nil? (source/re-match s #"e"))))))

(deftest line-tracking-test
  (testing "keeps line unchanged when not at a newline"
    (let [s (source/of "example")]
      (source/skip! s)
      (is (= 1 (source/index s)))
      (let [pos (source/position s)]
        (is (= 0 (pos/line-index pos)))
        (is (= 1 (pos/column-index pos))))))

  (testing "tracks Unix-style newlines"
    (let [s (source/of "\n")]
      (source/skip! s)
      (is (= 1 (source/index s)))
      (let [pos (source/position s)]
        (is (= 1 (pos/line-index pos)))
        (is (= 0 (pos/column-index pos))))))

  (testing "tracks Mac-style newlines"
    (let [s (source/of "\r")]
      (source/skip! s)
      (is (= 1 (source/index s)))
      (let [pos (source/position s)]
        (is (= 1 (pos/line-index pos)))
        (is (= 0 (pos/column-index pos))))))

  (testing "tracks Windows-style newlines"
    (let [s (source/of "\r\n")]
      (source/skip! s)
      (is (= 1 (source/index s)))
      (let [pos (source/position s)]
        (is (= 0 (pos/line-index pos)))
        (is (= 1 (pos/column-index pos))))
      (source/skip! s)
      (is (= 2 (source/index s)))
      (let [pos (source/position s)]
        (is (= 1 (pos/line-index pos)))
        (is (= 0 (pos/column-index pos))))))

  (testing "line tracking disabled"
    (let [s (source/of "a\nb\nc\n" {:line-tracking? false})]
      (source/skip! s 6)
      (let [pos (source/position s)]
        (is (= 0 (pos/line-index pos)))
        (is (= 6 (pos/column-index pos)))))))
