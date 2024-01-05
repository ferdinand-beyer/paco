(ns paco.detail.char-preds
  (:refer-clojure :exclude [test])
  #?@(:bb  [(:require [clojure.string :as str])]
      :clj [(:import [paco.detail.jvm CharPredicate])]
      :cljs [(:require [clojure.string :as str])]))

#?(:clj (set! *warn-on-reflection* true))

(defn code-point ^long [ch]
  #?(:clj  (unchecked-int (.charValue ^Character ch))
     :cljs (.charCodeAt ^js ch 0)))

(defn pred [x]
  #?(:bb   x
     :cljs x
     :clj  (CharPredicate/of x)))

(defn test
  #?(:clj {:inline (fn [pred ch]
                     (list '.test (vary-meta pred assoc :tag 'paco.detail.jvm.CharPredicate) ch))})
  [pred ch]
  #?(:bb   (pred ch)
     :cljs (pred ch)
     :clj  (.test ^CharPredicate pred ch)))

(defn eq [ch]
  #?(:bb   #(= ch %)
     :cljs #(= ch %)
     :clj  (CharPredicate/equals ch)))

(defn neq [ch]
  #?(:bb   #(not= ch %)
     :clj  (CharPredicate/equals ch)
     :cljs #(not= ch %)))

(defn not_ [pred]
  #?(:bb   (complement pred)
     :cljs (complement pred)
     :clj  (CharPredicate/not pred)))

(defn and_ [p1 p2]
  #?(:bb   (every-pred p1 p2)
     :cljs (every-pred p1 p2)
     :clj  (CharPredicate/and p1 p2)))

(defn or_ [p1 p2]
  #?(:bb   (some-fn p1 p2)
     :cljs (some-fn p1 p2)
     :clj  (CharPredicate/or p1 p2)))

(defn among [chars]
  #?(:bb   #(str/index-of chars %)
     :cljs #(str/index-of chars %)
     :clj  (CharPredicate/among chars)))

(defn not-among [chars]
  #?(:bb   (not_ (among chars))
     :cljs (not_ (among chars))
     :clj  (CharPredicate/notAmong chars)))

(defn in-range [min max]
  #?(:bb   #(<= (code-point min) (code-point %) (code-point max))
     :cljs #(<= (code-point min) (code-point %) (code-point max))
     :clj  (CharPredicate/inRange min max)))

(defn not-in-range [min max]
  #?(:bb   (not_ (in-range min max))
     :cljs (not_ (in-range min max))
     :clj  (CharPredicate/notInRange min max)))

(def ascii-upper?
  "Returns true if `ch` is a ASCII upper-case letter (A-Z)."
  #?(:bb   (in-range \A \Z)
     :cljs (in-range \A \Z)
     :clj  CharPredicate/ASCII_UPPER))

(def ascii-lower?
  "Returns true if `ch` is a ASCII lower-case letter (a-z)."
  #?(:bb   (in-range \a \z)
     :cljs (in-range \a \z)
     :clj  CharPredicate/ASCII_LOWER))

(def ascii-letter?
  "Returns true if `ch` is a ASCII letter (a-z, A-Z)."
  #?(:bb   (or_ ascii-upper? ascii-lower?)
     :cljs (or_ ascii-upper? ascii-lower?)
     :clj  CharPredicate/ASCII_LETTER))

;; https://unicode.org/reports/tr18/#General_Category_Property

(def upper?
  "Returns true if `ch` is a Unicode upper-case letter."
  #?(:bb   #(Character/isUpperCase (.charValue ^Character %))
     :clj  CharPredicate/UPPER
     :cljs #(.test #"(?u)^\p{Lu}$" %)))

(def lower?
  "Returns true if `ch` is a Unicode lower-case letter."
  #?(:bb   #(Character/isLowerCase (.charValue ^Character %))
     :clj  CharPredicate/LOWER
     :cljs #(.test #"(?u)^\p{Ll}$" %)))

(def letter?
  "Returns true if `ch` is a Unicode letter."
  #?(:bb   #(Character/isLetter (.charValue ^Character %))
     :clj CharPredicate/LETTER
     :cljs #(.test #"(?u)^\p{L}$" %)))

(def control?
  "Returns true if `ch` is a control character."
  #?(:bb   #(Character/isISOControl (.charValue ^Character %))
     :clj  CharPredicate/ISO_CONTROL
     :cljs #(.test #"(?u)^\p{Cc}$" %)))

(def digit?
  "Returns true if `ch` is a decimal digit (0-9)."
  #?(:bb   (in-range \0 \9)
     :cljs (in-range \0 \9)
     :clj  CharPredicate/DIGIT))

(def hex?
  "Returns true if `ch` is a hexadecimal digit (0-9, a-f, A-F)."
  #?(:bb   (or_ (in-range \0 \9) (or_ (in-range \a \f) (in-range \A \F)))
     :cljs (or_ (in-range \0 \9) (or_ (in-range \a \f) (in-range \A \F)))
     :clj  CharPredicate/HEX))

(def octal?
  "Returns true if `ch` is an octal digit (0-7)."
  #?(:bb   (in-range \0 \7)
     :cljs (in-range \0 \7)
     :clj  CharPredicate/OCTAL))
