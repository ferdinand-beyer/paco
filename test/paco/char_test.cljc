(ns paco.char-test
  (:require [clojure.test :refer [deftest is]]
            [paco.char :as c]
            [paco.core :as p]
            [paco.detail.error :as error]
            [paco.helper :as helper])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(deftest position-test
  (let [pos (p/parse c/position "")]
    (is (= 0 (c/line-index pos)))
    (is (= 0 (c/column-index pos))))
  (let [pos (p/parse (p/then (p/repeat p/any-token 6) c/position) "abc\ndef")]
    (is (= 1 (c/line-index pos)))
    (is (= 2 (c/column-index pos)))))

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

(deftest peek-char-test
  (let [reply (helper/run (c/peek-char \a) "abc")]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= \a (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (c/peek-char \a) "bc")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected-input \a) (error/unexpected-input \b)}
           (error/message-set (:error reply))))))

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

(deftest satisfy-test
  (is (= \x (p/parse (c/satisfy #(= \x %)) "xyz")))
  (let [reply (helper/run (c/satisfy #(= \x %) "the letter X"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected "the letter X") error/unexpected-end} (:messages reply)))))

(deftest ascii-upper-test
  (is (= \F (p/parse c/ascii-upper "Foobar")))
  (is (:fail? (helper/run c/ascii-upper "foobar"))))

(deftest ascii-lower-test
  (is (= \f (p/parse c/ascii-lower "foobar")))
  (is (:fail? (helper/run c/ascii-lower "FOOBAR"))))

(deftest ascii-letter-test
  (is (= \f (p/parse c/ascii-letter "foobar")))
  (is (:fail? (helper/run c/ascii-letter "123"))))

(deftest upper-test
  (is (= \Ä (p/parse c/upper "Ärmel")))
  (is (:fail? (helper/run c/upper "ärmel"))))

(deftest lower-test
  (is (= \ä (p/parse c/lower "ärmel")))
  (is (:fail? (helper/run c/lower "ÄRMEL"))))

(deftest letter-test
  (is (= \ß (p/parse c/letter "ßigkeit")))
  (is (:fail? (helper/run c/letter "123"))))

(deftest digit-test
  (is (= \5 (p/parse c/digit "5 o'clock")))
  (is (:fail? (helper/run c/digit "abc"))))

(deftest hex-test
  (is (= \c (p/parse c/hex "c0ffee")))
  (is (:fail? (helper/run c/hex "xyz"))))

(deftest octal-test
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

(deftest string-ci-test
  (is (= "FoO" (p/parse (c/string-ci "foo") "FoObAr")))
  (is (thrown? ExceptionInfo (c/string-ci "enter\n")))

  (let [reply (helper/run (c/string-ci "foo"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{error/unexpected-end (error/expected-input "foo")} (:messages reply))))

  (let [reply (helper/run (c/string-ci "foo") "FOX")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected-input "foo") (:error reply)))))

(deftest string-return-test
  (is (= ::ok (p/parse (c/string-return "ok" ::ok) "okay"))))

(deftest regex-test
  (is (= "foob" (p/parse (c/regex #"[b-x]+") "foobar")))
  (is (= "bar" (p/parse (p/then c/any-char c/any-char c/any-char
                                (c/regex #"\w+"))
                        "foobar and barfoo")))
  (is (= "foo" (p/parse (c/regex #"(f)(o)(o)") "foobar")))

  (let [reply (helper/run (c/regex #"[a-z]+"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/expected "pattern '[a-z]+'") error/unexpected-end}
           (:messages reply))))

  (is (= "b" (p/parse (p/then c/any-char (c/regex #"(?<=a)b"))
                      "abc"))
      "supports lookbehinds to previous position")

  (let [reply (helper/run (p/then c/any-char (c/regex #"^b")) "abc")]
    (is (:fail? reply) "does not match anchoring bounds")))

(deftest regex-groups-test
  (is (= ["foobar" "foo" "bar"] (p/parse (c/regex-groups #"(.{3})(.+)") "foobar"))))

(deftest str*-test
  (is (= "" (p/parse (c/*str (c/string "foo")) "bar")))
  (is (= "fb" (p/parse (c/*str (p/alt (c/string-return "foo" \f)
                                      (c/string-return "bar" "b")))
                       "foobar"))))

(deftest str+-test
  (is (= "fb" (p/parse (c/+str (p/alt (c/string-return "foo" \f)
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
    (is (= 2 (c/line-index (:position reply))))))
