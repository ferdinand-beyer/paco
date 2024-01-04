(ns paco.char-test
  (:require [clojure.test :refer [are deftest is]]
            [paco.char :as c]
            [paco.core :as p]
            [paco.detail.error :as error]
            [paco.detail.position :as pos]
            [paco.helper :as helper])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(deftest any-char-test
  (let [reply (helper/run c/any-char "xyz")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \x (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run c/any-char)]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-end (:error reply)))))

(deftest char-test
  (let [reply (helper/run (c/char \x) "xyz")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \x (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (c/char \x) "abc")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \a) (error/expected-input \x)} (:messages reply))))

  (let [reply (helper/run (c/char \x))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input \x)} (:messages reply)))))

(deftest char-return-test
  (let [reply (helper/run (c/char-return \5 5) "500")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= 5 (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (c/char-return \5 5))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input \5)} (:messages reply))))

  (let [reply (helper/run (c/char-return \5 5) "123")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \1) (error/expected-input \5)} (:messages reply)))))

(deftest any-of-test
  (is (= \z (p/parse (c/any-of "xyz") "zebra")))
  (let [reply (helper/run (c/any-of "xyz") "lion")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected-input \x)
             (error/expected-input \y)
             (error/expected-input \z)
             (error/unexpected-input \l)}
           (:messages reply)))))

(deftest none-of-test
  (is (= \l (p/parse (c/none-of "xyz") "lion")))
  (let [reply (helper/run (c/none-of "xyz") "zebra")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \z)} (:messages reply)))))

(deftest char-range-test
  (is (= \6 (p/parse (c/char-range \1 \9) "666")))
  (let [reply (helper/run (c/char-range \1 \9 "1-9") "08/15")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/unexpected-input \0) (error/expected "1-9")} (:messages reply)))))

(deftest match-test
  (is (= \x (p/parse (c/match #(= \x %)) "xyz")))
  (let [reply (helper/run (c/match #(= \x %) "the letter X"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected "the letter X") error/unexpected-end} (:messages reply)))))

(deftest ascii-upper-test
  (are [ch] (c/ascii-upper? ch) \A \B \M \Y \Z)
  (are [ch] (not (c/ascii-upper? ch)) \a \b \m \y \z \0 \? \u0000)

  (is (= \F (p/parse c/ascii-upper "Foobar")))
  (is (:fail? (helper/run c/ascii-upper "foobar"))))

(deftest ascii-lower-test
  (are [ch] (c/ascii-lower? ch) \a \b \m \y \z)
  (are [ch] (not (c/ascii-lower? ch)) \A \B \M \Y \Z \0 \? \u0000)
  (is (= \f (p/parse c/ascii-lower "foobar")))
  (is (:fail? (helper/run c/ascii-lower "FOOBAR"))))

(deftest ascii-letter-test
  (are [ch] (c/ascii-letter? ch) \a \b \m \y \z \A \B \M \Y \Z)
  (are [ch] (not (c/ascii-letter? ch)) \0 \? \u0000)
  (is (= \f (p/parse c/ascii-letter "foobar")))
  (is (:fail? (helper/run c/ascii-letter "123"))))

(deftest upper-test
  (are [ch] (c/upper? ch) \A \Æ)
  (are [ch] (not (c/upper? ch)) \a \æ \ß \0 \? \u0000)
  (is (= \Ä (p/parse c/upper "Ärmel")))
  (is (:fail? (helper/run c/upper "ärmel"))))

(deftest lower-test
  (are [ch] (c/lower? ch) \a \æ \ß)
  (are [ch] (not (c/lower? ch)) \A \Æ \0 \? \u0000)
  (is (= \ä (p/parse c/lower "ärmel")))
  (is (:fail? (helper/run c/lower "ÄRMEL"))))

(deftest letter-test
  (are [ch] (c/letter? ch) \A \Æ \a \æ \ß)
  (are [ch] (not (c/letter? ch)) \0 \? \u0000)
  (is (= \ß (p/parse c/letter "ßigkeit")))
  (is (:fail? (helper/run c/letter "123"))))

(deftest control-test
  (are [ch] (c/control? ch) \u0000 \u000f \u001f \u007f \u008f \u009f)
  (are [ch] (not (c/control? ch)) \A \Æ \a \æ \ß \0 \?))

(deftest digit-test
  (are [ch] (c/digit? ch) \0 \1 \5 \8 \9)
  (are [ch] (not (c/digit? ch)) \A \Z \a \z \? \u0000)
  (is (= \5 (p/parse c/digit "5 o'clock")))
  (is (:fail? (helper/run c/digit "abc"))))

(deftest hex-test
  (are [ch] (c/hex? ch) \0 \1 \9 \a \c \f \A \C \F)
  (are [ch] (not (c/hex? ch)) \G \g \Æ \æ \ß \? \u0000)
  (is (= \c (p/parse c/hex "c0ffee")))
  (is (:fail? (helper/run c/hex "xyz"))))

(deftest octal-test
  (are [ch] (c/octal? ch) \0 \1 \5 \7)
  (are [ch] (not (c/octal? ch)) \8 \9 \a \A \Æ \æ \ß \? \u0000)
  (is (= \0 (p/parse c/octal "0733")))
  (is (:fail? (helper/run c/octal "999"))))

(deftest string-test
  (is (= "foo" (p/parse (c/string "foo") "foobar")))
  (is (thrown? ExceptionInfo (c/string "enter\n")))

  (let [reply (helper/run (c/string "foo"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input "foo")} (:messages reply))))

  (let [reply (helper/run (c/string "foo") "FOO")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected-input "foo") (:error reply)))))

(deftest string-i-test
  (is (= "FoO" (p/parse (c/string-i "foo") "FoObAr")))
  (is (thrown? ExceptionInfo (c/string-i "enter\n")))

  (let [reply (helper/run (c/string-i "foo"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input "foo")} (:messages reply))))

  (let [reply (helper/run (c/string-i "foo") "FOX")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected-input "foo") (:error reply)))))

(deftest string-return-test
  (is (= ::ok (p/parse (c/string-return "ok" ::ok) "okay"))))

(deftest str*-test
  (is (= "" (p/parse (c/str* (c/string "foo")) "bar")))
  (is (= "fb" (p/parse (c/str* (p/alt (c/string-return "foo" \f)
                                      (c/string-return "bar" "b")))
                       "foobar"))))

(deftest str+-test
  (is (= "fb" (p/parse (c/str+ (p/alt (c/string-return "foo" \f)
                                      (c/string-return "bar" "b")))
                       "foobar"))))

(deftest strcat-test
  (is (= "0003.1415" (p/parse (c/strcat (p/+ c/digit)
                                        (c/char \.)
                                        (p/+ c/digit))
                              "0003.1415"))))

(deftest skipped-test
  (is (= "abc" (p/parse (c/skipped (p/* c/any-char)) "abc")))
  (let [reply (helper/run (c/skipped (p/* c/any-char)) "one\ntwo\n")]
    (is (= 2 (pos/line-index (:state reply))))))