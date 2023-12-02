(ns paco.chars-test
  (:require [clojure.test :refer [are deftest is]]
            [paco.chars :as chars]
            [paco.core :as p]))

;; TODO: Test failures.  First add error reporting utils

(deftest ascii-upper-test
  (are [ch] (chars/ascii-upper? ch) \A \B \M \Y \Z)
  (are [ch] (not (chars/ascii-upper? ch)) \a \b \m \y \z \0 \? \u0000)
  (is (= \F (p/parse chars/ascii-upper "Foobar"))))

(deftest ascii-lower-test
  (are [ch] (chars/ascii-lower? ch) \a \b \m \y \z)
  (are [ch] (not (chars/ascii-lower? ch)) \A \B \M \Y \Z \0 \? \u0000)
  (is (= \f (p/parse chars/ascii-lower "foobar"))))

(deftest ascii-letter-test
  (are [ch] (chars/ascii-letter? ch) \a \b \m \y \z \A \B \M \Y \Z)
  (are [ch] (not (chars/ascii-letter? ch)) \0 \? \u0000)
  (is (= \f (p/parse chars/ascii-letter "foobar"))))

(deftest upper-test
  (are [ch] (chars/upper? ch) \A \Æ)
  (are [ch] (not (chars/upper? ch)) \a \æ \ß \0 \? \u0000)
  (is (= \Ä (p/parse chars/upper "Ärmel"))))

(deftest lower-test
  (are [ch] (chars/lower? ch) \a \æ \ß)
  (are [ch] (not (chars/lower? ch)) \A \Æ \0 \? \u0000)
  (is (= \ä (p/parse chars/lower "ärmel"))))

(deftest letter-test
  (are [ch] (chars/letter? ch) \A \Æ \a \æ \ß)
  (are [ch] (not (chars/letter? ch)) \0 \? \u0000)
  (is (= \ß (p/parse chars/letter "ßigkeit"))))

(deftest control-test
  (are [ch] (chars/control? ch) \u0000 \u000f \u001f \u007f \u008f \u009f)
  (are [ch] (not (chars/control? ch)) \A \Æ \a \æ \ß \0 \?))

(deftest digit-test
  (are [ch] (chars/digit? ch) \0 \1 \5 \8 \9)
  (are [ch] (not (chars/digit? ch)) \A \Z \a \z \? \u0000)
  (is (= \5 (p/parse chars/digit "5 o'clock"))))

(deftest hex-test
  (are [ch] (chars/hex? ch) \0 \1 \9 \a \c \f \A \C \F)
  (are [ch] (not (chars/hex? ch)) \G \g \Æ \æ \ß \? \u0000)
  (is (= \c (p/parse chars/hex "c0ffee"))))

(deftest octal-test
  (are [ch] (chars/octal? ch) \0 \1 \5 \7)
  (are [ch] (not (chars/octal? ch)) \8 \9 \a \A\Æ \æ \ß \? \u0000)
  (is (= \0 (p/parse chars/octal "0733"))))

(deftest string-test
  (is (= "foo" (p/parse (chars/string "foo") "foobar"))))

(deftest string-i-test
  (is (= "FoO" (p/parse (chars/string-i "foo") "FoObAr"))))
