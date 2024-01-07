(ns paco.char.preds
  (:refer-clojure :exclude [and or not test])
  #?@(:bb   [(:require [clojure.string :as str])]
      :clj  [(:import [paco.detail.jvm ICharPredicate])]
      :cljs [(:require [clojure.string :as str])]))

#?(:clj (set! *warn-on-reflection* true))

(defn code-point ^long [ch]
  #?(:clj  (unchecked-int (.charValue ^Character ch))
     :cljs (.charCodeAt ^js ch 0)))

(defn pred
  #?(:clj {:inline (fn [x] `(ICharPredicate/of ~x))})
  [x]
  #?(:bb   x
     :cljs x
     :clj  (ICharPredicate/of x)))

(defn test
  #?(:clj {:inline (fn [pred ch]
                     (list '.test (vary-meta pred assoc :tag 'paco.detail.jvm.ICharPredicate) ch))})
  [pred ch]
  #?(:bb   (pred ch)
     :cljs (pred ch)
     :clj  (.test ^ICharPredicate pred ch)))

(defn eq [ch]
  #?(:bb   #(= ch %)
     :cljs #(= ch %)
     :clj  (ICharPredicate/equals ch)))

(defn neq [ch]
  #?(:bb   #(not= ch %)
     :clj  (ICharPredicate/notEquals ch)
     :cljs #(not= ch %)))

(defn not [pred]
  #?(:bb   (complement pred)
     :cljs (complement pred)
     :clj  (ICharPredicate/not pred)))

(defn and [p1 p2]
  #?(:bb   (every-pred p1 p2)
     :cljs (every-pred p1 p2)
     :clj  (ICharPredicate/and p1 p2)))

(defn or [p1 p2]
  #?(:bb   (some-fn p1 p2)
     :cljs (some-fn p1 p2)
     :clj  (ICharPredicate/or p1 p2)))

(defn among [chars]
  #?(:bb   #(str/index-of chars %)
     :cljs #(str/index-of chars %)
     :clj  (ICharPredicate/among chars)))

(defn not-among [chars]
  #?(:bb   (not (among chars))
     :cljs (not (among chars))
     :clj  (ICharPredicate/notAmong chars)))

(defn in-range [min max]
  #?(:bb   #(<= (code-point min) (code-point %) (code-point max))
     :cljs #(<= (code-point min) (code-point %) (code-point max))
     :clj  (ICharPredicate/inRange min max)))

(defn not-in-range [min max]
  #?(:bb   (not (in-range min max))
     :cljs (not (in-range min max))
     :clj  (ICharPredicate/notInRange min max)))

(def ascii-upper?
  "Returns true if `ch` is a ASCII upper-case letter (A-Z)."
  #?(:bb   (in-range \A \Z)
     :cljs (in-range \A \Z)
     :clj  ICharPredicate/ASCII_UPPER))

(def ascii-lower?
  "Returns true if `ch` is a ASCII lower-case letter (a-z)."
  #?(:bb   (in-range \a \z)
     :cljs (in-range \a \z)
     :clj  ICharPredicate/ASCII_LOWER))

(def ascii-letter?
  "Returns true if `ch` is a ASCII letter (a-z, A-Z)."
  #?(:bb   (or ascii-upper? ascii-lower?)
     :cljs (or ascii-upper? ascii-lower?)
     :clj  ICharPredicate/ASCII_LETTER))

;; https://unicode.org/reports/tr18/#General_Category_Property

(def upper?
  "Returns true if `ch` is a Unicode upper-case letter."
  #?(:bb   #(Character/isUpperCase (.charValue ^Character %))
     :clj  ICharPredicate/UPPER
     :cljs #(.test #"(?u)^\p{Lu}$" %)))

(def lower?
  "Returns true if `ch` is a Unicode lower-case letter."
  #?(:bb   #(Character/isLowerCase (.charValue ^Character %))
     :clj  ICharPredicate/LOWER
     :cljs #(.test #"(?u)^\p{Ll}$" %)))

(def letter?
  "Returns true if `ch` is a Unicode letter."
  #?(:bb   #(Character/isLetter (.charValue ^Character %))
     :clj ICharPredicate/LETTER
     :cljs #(.test #"(?u)^\p{L}$" %)))

(def control?
  "Returns true if `ch` is a control character."
  #?(:bb   #(Character/isISOControl (.charValue ^Character %))
     :clj  ICharPredicate/ISO_CONTROL
     :cljs #(.test #"(?u)^\p{Cc}$" %)))

(def digit?
  "Returns true if `ch` is a decimal digit (0-9)."
  #?(:bb   (in-range \0 \9)
     :cljs (in-range \0 \9)
     :clj  ICharPredicate/DIGIT))

(def hex?
  "Returns true if `ch` is a hexadecimal digit (0-9, a-f, A-F)."
  #?(:bb   (or (in-range \0 \9) (or (in-range \a \f) (in-range \A \F)))
     :cljs (or (in-range \0 \9) (or (in-range \a \f) (in-range \A \F)))
     :clj  ICharPredicate/HEX))

(def octal?
  "Returns true if `ch` is an octal digit (0-7)."
  #?(:bb   (in-range \0 \7)
     :cljs (in-range \0 \7)
     :clj  ICharPredicate/OCTAL))
