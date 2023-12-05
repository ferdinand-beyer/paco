(ns paco.chars
  (:refer-clojure :exclude [char])
  (:require [paco.core :as p]
            [paco.detail :as detail]
            [paco.error :as error]
            [paco.state :as state])
  #?(:cljs (:require-macros [paco.chars :refer [test-ranges]])))

;;---------------------------------------------------------
;; Character predicates

(defn code-point
  "Returns the Unicode code point of `ch` as an integer."
  [ch]
  #?(:clj  (unchecked-int (.charValue ^Character ch))
     :cljs (.charCodeAt ^js ch 0)))

(defmacro ^:private test-ranges [ch & ranges]
  (let [cp (gensym "cp")]
    `(let [~cp (code-point ~ch)]
       (or ~@(for [[min-ch max-ch] ranges]
               `(and (<= ~(code-point min-ch) ~cp)
                     (<= ~cp ~(code-point max-ch))))))))

(defn ascii-upper?
  "Returns true if `ch` is a ASCII upper-case letter (A-Z)."
  [ch]
  (test-ranges ch [\A \Z]))

(defn ascii-lower?
  "Returns true if `ch` is a ASCII lower-case letter (a-z)."
  [ch]
  (test-ranges ch [\a \z]))

(defn ascii-letter?
  "Returns true if `ch` is a ASCII letter (a-z, A-Z)."
  [ch]
  (test-ranges ch [\a \z] [\A \Z]))

(defn upper?
  "Returns true if `ch` is a Unicode upper-case letter."
  [ch]
  #?(:clj  (Character/isUpperCase (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Lu}$" ch)))

(defn lower?
  "Returns true if `ch` is a Unicode lower-case letter."
  [ch]
  #?(:clj  (Character/isLowerCase (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Ll}$" ch)))

(defn letter?
  "Returns true if `ch` is a Unicode letter."
  [ch]
  #?(:clj  (Character/isLetter (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{L}$" ch)))

(defn control?
  "Returns true if `ch` is a control character."
  [ch]
  #?(:clj  (Character/isISOControl (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Cc}$" ch)))

(defn digit?
  "Returns true if `ch` is a decimal digit (0-9)."
  [ch]
  (test-ranges ch [\0 \9]))

(defn hex?
  "Returns true if `ch` is a hexadecimal digit (0-9, a-f, A-F)."
  [ch]
  (test-ranges ch [\0 \9] [\a \f] [\A \F]))

(defn octal?
  "Returns true if `ch` is an octal digit (0-7)."
  [ch]
  (test-ranges ch [\0 \7]))

;;---------------------------------------------------------
;; Character parsers

(defn- match-char [pred skip error]
  (fn [state reply]
    (if-let [ch (state/peek state)]
      (if (pred ch)
        (reply detail/ok (skip state) ch nil)
        (reply detail/error state nil (error (error/unexpected-input ch))))
      (reply detail/error state nil (error error/unexpected-eof)))))

;; fparsec: satisfy; normalises newlines
(defn match
  ([pred]
   (match pred nil))
  ([pred label]
   (match-char pred state/skip-char
               (if label
                 (let [error (error/expected label)]
                   #(error/merge error %))
                 identity))))

(defn char-return [ch value]
  ;; TODO: optimise for non-newline
  (let [error (error/expected-input ch)]
    (fn [state reply]
      (if-let [next-ch (state/peek state)]
        (if (= ch next-ch)
          (reply detail/ok (state/skip-char state) value nil)
          (reply detail/error state nil (error/merge (error/unexpected-input next-ch) error)))
        (reply detail/error state nil (error/merge error/unexpected-eof error))))))

;; fparsec: pchar
(defn char [ch]
  (char-return ch ch))

(defn skip-char [ch]
  (char-return ch nil))

;; fparsec: + skipAnyChar
(def any-char
  (fn [state reply]
    (if-let [ch (state/peek state)]
      (reply detail/ok (state/skip-char state) ch nil)
      (reply detail/error state nil error/unexpected-eof))))

;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Optimise for newlines?
  (let [error (map error/expected-input chars)]
    (match-char (set chars) state/skip-char #(error/merge error %))))

(defn none-of [chars]
  ;; TODO: (expected "any char not in ...")
  (match-char (complement (set chars)) state/skip-char identity))

(def ascii-upper
  (match ascii-upper? "ASCII upper-case letter"))

(def ascii-lower
  (match ascii-lower? "ASCII lower-case letter"))

(def ascii-letter
  (match ascii-letter? "ASCII letter"))

(def upper
  (match upper? "upper-case letter"))

(def lower
  (match lower? "lower-case letter"))

(def letter
  (match letter? "letter"))

(def digit
  (match digit? "decimal digit"))

(def hex
  (match hex? "hexadecimal digit"))

(def octal
  (match octal? "octal digit"))

;; fparsec: tab
;; fparsec: newline, skipNewline, newlineReturn + unicode variants
;; fparsec: spaces, spaces1 + unicode variants

;;---------------------------------------------------------
;; String parsers

(defn- check-string-literal [s]
  (when (empty? s)
    (throw (ex-info "String literal cannot be empty" {})))
  (when (re-find #"[\r\n]" s)
    (throw (ex-info "String literal cannot contain newlines" {}))))

;; fparsec: pstring
;; fparsec: + skipString, stringReturn, CI variants,
;;   anyString, skipAnyString
(defn string [s]
  (check-string-literal s)
  (let [length   (count s)
        error     (error/expected-input s)
        error-eof (error/merge error/unexpected-eof error)]
    (fn [state reply]
      (if (state/matches-str? state s)
        (reply detail/ok (state/skip state length) s nil)
        (reply detail/error state nil (if (state/at-end? state)
                                       error-eof
                                       error))))))

(defn string-return [ch x]
  (p/return (string ch) x))

(defn string-i [s]
  (check-string-literal s)
  (let [length    (count s)
        error     (error/expected-input s) ;; TODO: expected-str-ic?
        error-eof (error/merge error/unexpected-eof error)]
    (fn [state reply]
      (if (state/matches-str-i? state s)
        (reply detail/ok (state/skip state length)
               (state/peek-str state length) nil)
        (reply detail/error state nil (if (state/at-end? state)
                                       error-eof
                                       error))))))

;; fparsec: restOfLine, skipRestOfLine
;; fparsec: charsTillString
#_(defn chars-till-str [s skip? max-count])

;; fparsec: manySatisfy
;; fparsec: manySatisfy2 -- different pred for first character
;; fparsec: many1Satisfy: one or more
;; fparsec: manyMinMaxSatisfy
#_(defn match*
    "Parses a sequence of zero or more characters satisfying `pred`,
   and returns them as a string."
    [pred])

;; fparsec: regex
;; fparsec: identifier

;; fparsec: combine parsers to return strings
;;   manyChars, manyStrings, skipped

;; fparsec: number parsers (int, float)
