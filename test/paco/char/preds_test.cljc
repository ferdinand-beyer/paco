(ns paco.char.preds-test
  (:require [clojure.test :refer [are deftest is]]
            [paco.char.preds :as preds]))

(deftest ascii-upper?-test
  (are [ch] (preds/test preds/ascii-upper? ch) \A \B \M \Y \Z)
  (are [ch] (not (preds/test preds/ascii-upper? ch)) \a \b \m \y \z \0 \? \u0000))

(deftest ascii-lower?-test
  (are [ch] (preds/test preds/ascii-lower? ch) \a \b \m \y \z)
  (are [ch] (not (preds/test preds/ascii-lower? ch)) \A \B \M \Y \Z \0 \? \u0000))

(deftest ascii-letter?-test
  (are [ch] (preds/test preds/ascii-letter? ch) \a \b \m \y \z \A \B \M \Y \Z)
  (are [ch] (not (preds/test preds/ascii-letter? ch)) \0 \? \u0000))

(deftest upper?-test
  (are [ch] (preds/test preds/upper? ch) \A \Æ)
  (are [ch] (not (preds/test preds/upper? ch)) \a \æ \ß \0 \? \u0000))

(deftest lower?-test
  (are [ch] (preds/test preds/lower? ch) \a \æ \ß)
  (are [ch] (not (preds/test preds/lower? ch)) \A \Æ \0 \? \u0000))

(deftest letter?-test
  (are [ch] (preds/test preds/letter? ch) \A \Æ \a \æ \ß)
  (are [ch] (not (preds/test preds/letter? ch)) \0 \? \u0000))

(deftest control?-test
  (are [ch] (preds/test preds/control? ch) \u0000 \u000f \u001f \u007f \u008f \u009f)
  (are [ch] (not (preds/test preds/control? ch)) \A \Æ \a \æ \ß \0 \?))

(deftest digit?-test
  (are [ch] (preds/test preds/digit? ch) \0 \1 \5 \8 \9)
  (are [ch] (not (preds/test preds/digit? ch)) \A \Z \a \z \? \u0000))

(deftest hex?-test
  (are [ch] (preds/test preds/hex? ch) \0 \1 \9 \a \c \f \A \C \F)
  (are [ch] (not (preds/test preds/hex? ch)) \G \g \Æ \æ \ß \? \u0000))

(deftest octal?-test
  (are [ch] (preds/test preds/octal? ch) \0 \1 \5 \7)
  (are [ch] (not (preds/test preds/octal? ch)) \8 \9 \a \A \Æ \æ \ß \? \u0000))

(deftest and-test
  (is (preds/test (preds/and preds/digit?) \7))
  (let [p (preds/and preds/digit? preds/octal?)]
    (is (preds/test p \7))
    (is (not (preds/test p \9))))
  (let [p (preds/and preds/digit? preds/octal? #(= \5 %))]
    (is (preds/test p \5))
    (is (not (preds/test p \4)))))

(deftest or-test
  (is (preds/test (preds/or preds/digit?) \7))
  (let [p (preds/or preds/digit? preds/letter?)]
    (is (preds/test p \7))
    (is (preds/test p \x))
    (is (not (preds/test p \space))))
  (let [p (preds/or preds/digit? preds/letter? (preds/among "_-$"))]
    (is (preds/test p \7))
    (is (preds/test p \x))
    (is (preds/test p \$))
    (is (not (preds/test p \newline)))))
