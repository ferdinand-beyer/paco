(ns paco.detail.char-preds-test
  (:require [clojure.test :refer [are deftest]]
            [paco.detail.char-preds :as char-preds]))

(deftest ascii-upper?-test
  (are [ch] (char-preds/ascii-upper? ch) \A \B \M \Y \Z)
  (are [ch] (not (char-preds/ascii-upper? ch)) \a \b \m \y \z \0 \? \u0000))

(deftest ascii-lower?-test
  (are [ch] (char-preds/ascii-lower? ch) \a \b \m \y \z)
  (are [ch] (not (char-preds/ascii-lower? ch)) \A \B \M \Y \Z \0 \? \u0000))

(deftest ascii-letter?-test
  (are [ch] (char-preds/ascii-letter? ch) \a \b \m \y \z \A \B \M \Y \Z)
  (are [ch] (not (char-preds/ascii-letter? ch)) \0 \? \u0000))

(deftest upper?-test
  (are [ch] (char-preds/upper? ch) \A \Æ)
  (are [ch] (not (char-preds/upper? ch)) \a \æ \ß \0 \? \u0000))

(deftest lower?-test
  (are [ch] (char-preds/lower? ch) \a \æ \ß)
  (are [ch] (not (char-preds/lower? ch)) \A \Æ \0 \? \u0000))

(deftest letter?-test
  (are [ch] (char-preds/letter? ch) \A \Æ \a \æ \ß)
  (are [ch] (not (char-preds/letter? ch)) \0 \? \u0000))

(deftest control?-test
  (are [ch] (char-preds/control? ch) \u0000 \u000f \u001f \u007f \u008f \u009f)
  (are [ch] (not (char-preds/control? ch)) \A \Æ \a \æ \ß \0 \?))

(deftest digit?-test
  (are [ch] (char-preds/digit? ch) \0 \1 \5 \8 \9)
  (are [ch] (not (char-preds/digit? ch)) \A \Z \a \z \? \u0000))

(deftest hex?-test
  (are [ch] (char-preds/hex? ch) \0 \1 \9 \a \c \f \A \C \F)
  (are [ch] (not (char-preds/hex? ch)) \G \g \Æ \æ \ß \? \u0000))

(deftest octal?-test
  (are [ch] (char-preds/octal? ch) \0 \1 \5 \7)
  (are [ch] (not (char-preds/octal? ch)) \8 \9 \a \A \Æ \æ \ß \? \u0000))
