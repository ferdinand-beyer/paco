(ns paco.chars-test
  (:require [clojure.test :refer [are deftest is]]
            [paco.chars :as chars]
            [paco.core :as p]
            [paco.error :as error]
            [paco.helper :as helper]
            [paco.pos :as pos])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(deftest any-char-test
  (let [reply (helper/run chars/any-char "xyz")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \x (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run chars/any-char)]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-end (:error reply)))))

(deftest char-test
  (let [reply (helper/run (chars/char \x) "xyz")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \x (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (chars/char \x) "abc")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \a) (error/expected-input \x)} (:messages reply))))

  (let [reply (helper/run (chars/char \x))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input \x)} (:messages reply)))))

(deftest char-return-test
  (let [reply (helper/run (chars/char-return \5 5) "500")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= 5 (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (chars/char-return \5 5))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input \5)} (:messages reply))))

  (let [reply (helper/run (chars/char-return \5 5) "123")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \1) (error/expected-input \5)} (:messages reply)))))

(deftest any-of-test
  (is (= \z (p/parse (chars/any-of "xyz") "zebra")))
  (let [reply (helper/run (chars/any-of "xyz") "lion")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected-input \x)
             (error/expected-input \y)
             (error/expected-input \z)
             (error/unexpected-input \l)}
           (:messages reply)))))

(deftest none-of-test
  (is (= \l (p/parse (chars/none-of "xyz") "lion")))
  (let [reply (helper/run (chars/none-of "xyz") "zebra")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \z)} (:messages reply)))))

(deftest char-range-test
  (is (= \6 (p/parse (chars/char-range \1 \9) "666")))
  (let [reply (helper/run (chars/char-range \1 \9 "1-9") "08/15")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \0) (error/expected "1-9")} (:messages reply)))))

(deftest match-test
  (is (= \x (p/parse (chars/match #(= \x %)) "xyz")))
  (let [reply (helper/run (chars/match #(= \x %) "the letter X"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected "the letter X") error/unexpected-end} (:messages reply)))))

(deftest ascii-upper-test
  (are [ch] (chars/ascii-upper? ch) \A \B \M \Y \Z)
  (are [ch] (not (chars/ascii-upper? ch)) \a \b \m \y \z \0 \? \u0000)

  (is (= \F (p/parse chars/ascii-upper "Foobar")))
  (is (:fail? (helper/run chars/ascii-upper "foobar"))))

(deftest ascii-lower-test
  (are [ch] (chars/ascii-lower? ch) \a \b \m \y \z)
  (are [ch] (not (chars/ascii-lower? ch)) \A \B \M \Y \Z \0 \? \u0000)
  (is (= \f (p/parse chars/ascii-lower "foobar")))
  (is (:fail? (helper/run chars/ascii-lower "FOOBAR"))))

(deftest ascii-letter-test
  (are [ch] (chars/ascii-letter? ch) \a \b \m \y \z \A \B \M \Y \Z)
  (are [ch] (not (chars/ascii-letter? ch)) \0 \? \u0000)
  (is (= \f (p/parse chars/ascii-letter "foobar")))
  (is (:fail? (helper/run chars/ascii-letter "123"))))

(deftest upper-test
  (are [ch] (chars/upper? ch) \A \Æ)
  (are [ch] (not (chars/upper? ch)) \a \æ \ß \0 \? \u0000)
  (is (= \Ä (p/parse chars/upper "Ärmel")))
  (is (:fail? (helper/run chars/upper "ärmel"))))

(deftest lower-test
  (are [ch] (chars/lower? ch) \a \æ \ß)
  (are [ch] (not (chars/lower? ch)) \A \Æ \0 \? \u0000)
  (is (= \ä (p/parse chars/lower "ärmel")))
  (is (:fail? (helper/run chars/lower "ÄRMEL"))))

(deftest letter-test
  (are [ch] (chars/letter? ch) \A \Æ \a \æ \ß)
  (are [ch] (not (chars/letter? ch)) \0 \? \u0000)
  (is (= \ß (p/parse chars/letter "ßigkeit")))
  (is (:fail? (helper/run chars/letter "123"))))

(deftest control-test
  (are [ch] (chars/control? ch) \u0000 \u000f \u001f \u007f \u008f \u009f)
  (are [ch] (not (chars/control? ch)) \A \Æ \a \æ \ß \0 \?))

(deftest digit-test
  (are [ch] (chars/digit? ch) \0 \1 \5 \8 \9)
  (are [ch] (not (chars/digit? ch)) \A \Z \a \z \? \u0000)
  (is (= \5 (p/parse chars/digit "5 o'clock")))
  (is (:fail? (helper/run chars/digit "abc"))))

(deftest hex-test
  (are [ch] (chars/hex? ch) \0 \1 \9 \a \c \f \A \C \F)
  (are [ch] (not (chars/hex? ch)) \G \g \Æ \æ \ß \? \u0000)
  (is (= \c (p/parse chars/hex "c0ffee")))
  (is (:fail? (helper/run chars/hex "xyz"))))

(deftest octal-test
  (are [ch] (chars/octal? ch) \0 \1 \5 \7)
  (are [ch] (not (chars/octal? ch)) \8 \9 \a \A \Æ \æ \ß \? \u0000)
  (is (= \0 (p/parse chars/octal "0733")))
  (is (:fail? (helper/run chars/octal "999"))))

(deftest string-test
  (is (= "foo" (p/parse (chars/string "foo") "foobar")))
  (is (thrown? ExceptionInfo (chars/string "enter\n")))

  (let [reply (helper/run (chars/string "foo"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input "foo")} (:messages reply))))

  (let [reply (helper/run (chars/string "foo") "FOO")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected-input "foo") (:error reply)))))

(deftest string-i-test
  (is (= "FoO" (p/parse (chars/string-i "foo") "FoObAr")))
  (is (thrown? ExceptionInfo (chars/string-i "enter\n")))

  (let [reply (helper/run (chars/string-i "foo"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input "foo")} (:messages reply))))

  (let [reply (helper/run (chars/string-i "foo") "FOX")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected-input "foo") (:error reply)))))

(deftest string-return-test
  (is (= ::ok (p/parse (chars/string-return "ok" ::ok) "okay"))))

(deftest str*-test
  (is (= "" (p/parse (chars/str* (chars/string "foo")) "bar")))
  (is (= "fb" (p/parse (chars/str* (p/alt (chars/string-return "foo" \f)
                                          (chars/string-return "bar" "b")))
                       "foobar"))))

(deftest str+-test
  (is (= "fb" (p/parse (chars/str+ (p/alt (chars/string-return "foo" \f)
                                          (chars/string-return "bar" "b")))
                       "foobar"))))

(deftest skipped-test
  (is (= "abc" (p/parse (chars/skipped (p/* chars/any-char)) "abc")))
  (let [reply (helper/run (chars/skipped (p/* chars/any-char)) "one\ntwo\n")]
    (is (= 2 (pos/line-index (:state reply))))))
