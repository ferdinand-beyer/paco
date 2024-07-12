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
  "Coerce `x` to a character predicate."
  #?(:clj {:inline (fn [x] `(ICharPredicate/of ~x))})
  [x]
  #?(:bb   x
     :cljs x
     :clj  (ICharPredicate/of x)))

(defn test
  "Tests `ch` against the character predicate `pred`."
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

(defn not [p]
  #?(:bb   (complement p)
     :cljs (complement p)
     :clj  (ICharPredicate/not (pred p))))

(def and
  #?(:bb   every-pred
     :cljs every-pred
     :clj  (fn
             ([p] (pred p))
             ([p1 p2] (ICharPredicate/and (pred p1) (pred p2)))
             ([p1 p2 & ps]
              (reduce (fn [p x]
                        (ICharPredicate/and p (pred x)))
                      (ICharPredicate/and (pred p1) (pred p2))
                      ps)))))

(def or
  #?(:bb   some-fn
     :cljs some-fn
     :clj  (fn
             ([p] (pred p))
             ([p1 p2] (ICharPredicate/or (pred p1) (pred p2)))
             ([p1 p2 & ps]
              (reduce (fn [p x]
                        (ICharPredicate/or p (pred x)))
                      (ICharPredicate/or (pred p1) (pred p2))
                      ps)))))

(defn any-of [chars]
  #?(:bb   #(str/index-of chars %)
     :cljs #(str/index-of chars %)
     :clj  (ICharPredicate/anyOf chars)))

(defn none-of [chars]
  #?(:bb   (not (any-of chars))
     :cljs (not (any-of chars))
     :clj  (ICharPredicate/noneOf chars)))

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
  (in-range \A \Z))

(def ascii-lower?
  "Returns true if `ch` is a ASCII lower-case letter (a-z)."
  (in-range \a \z))

(def ascii-letter?
  "Returns true if `ch` is a ASCII letter (a-z, A-Z)."
  (or ascii-upper? ascii-lower?))

(def space?
  "Returns true if `ch` is a common whitespace character: space, tabulator,
   newline or carriage return."
  (any-of " \t\n\r"))

(def ^:private additional-unicode-space?
  "Characters considered whitespace by the JVM and .NET."
  (any-of "\t\n\u000b\f\r"))

(def unicode-space?
  "Returns true if `ch` is a Unicode space character (any space separator,
   line separator, or paragraph separator)."
  #?(:bb   (or #(Character/isSpaceChar (.charValue ^Character %))
               additional-unicode-space?)
     :clj  (or ICharPredicate/SPACE additional-unicode-space?)
     :cljs (or #(.test #"(?u)^\s$" %) additional-unicode-space?)))

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
  (in-range \0 \9))

(def hex?
  "Returns true if `ch` is a hexadecimal digit (0-9, a-f, A-F)."
  (or digit? (in-range \a \f) (in-range \A \F)))

(def octal?
  "Returns true if `ch` is an octal digit (0-7)."
  (in-range \0 \7))
