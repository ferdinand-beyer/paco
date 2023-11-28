(ns paco.chars
  (:refer-clojure :exclude [char])
  (:require [paco.core :as p]
            [paco.error :as error]
            [paco.state :as state])
  #?(:cljs (:require-macros [paco.chars :refer [test-ranges]])))

;;---------------------------------------------------------
;; Character predicates

(defn- code-point [ch]
  #?(:clj  (unchecked-int (.charValue ^Character ch))
     :cljs (.charCodeAt ^js ch 0)))

(defmacro ^:private test-ranges [ch & ranges]
  (let [cp (gensym "cp")]
    `(let [~cp (code-point ~ch)]
       (or ~@(for [[min-ch max-ch] ranges]
               `(and (<= ~(code-point min-ch) ~cp)
                     (<= ~cp ~(code-point max-ch))))))))

(defn ascii-upper? [ch]
  (test-ranges ch [\A \Z]))

(defn ascii-lower? [ch]
  (test-ranges ch [\a \z]))

(defn ascii-letter? [ch]
  (test-ranges ch [\a \z] [\A \Z]))

(defn upper? [ch]
  #?(:clj  (Character/isUpperCase (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Lu}$" ch)))

(defn lower? [ch]
  #?(:clj  (Character/isLowerCase (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Ll}$" ch)))

(defn letter? [ch]
  #?(:clj  (Character/isLetter (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{L}$" ch)))

(defn control? [ch]
  #?(:clj  (Character/isISOControl (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Cc}$" ch)))

;; decimal digit
(defn digit? [ch]
  (test-ranges ch [\0 \9]))

(defn hex? [ch]
  (test-ranges ch [\0 \9] [\a \f] [\A \F]))

(defn octal? [ch]
  (test-ranges ch [\0 \7]))

;;---------------------------------------------------------
;; Character parsers

;; fparsec: satisfy; normalises newlines
(defn match
  ([pred]
   (match pred nil))
  ([pred label]
   (let [wrap-msgs (if label
                     (let [msg (first (error/expected label))]
                       #(cons msg %))
                     identity)]
     (fn [state _ ok! fail _]
       (if-let [ch (state/peek state)]
         (if (pred ch)
           (ok! (state/skip-char state) ch nil)
           (fail state (wrap-msgs (error/unexpected-str (str ch)))))
         (fail state (wrap-msgs error/unexpected-eof)))))))

(defn char-return [ch value]
  ;; TODO: optimise for non-newline
  (let [msgs (error/expected-str (str ch))]
    (fn [state _ ok! fail _]
      (if-let [ch' (state/peek state)]
        (if (= ch ch')
          (ok! (state/skip-char state) value nil)
          (fail state (error/merge-messages (error/unexpected-str (str ch')) msgs)))
        (fail state (error/merge-messages error/unexpected-eof msgs))))))

;; fparsec: pchar
(defn char [ch]
  (char-return ch ch))

(defn skip-char [ch]
  (char-return ch nil))

;; fparsec: + skipAnyChar
(def any-char
  (fn [state _ ok! fail _]
    (if-let [ch (state/peek state)]
      (ok! (state/skip-char state) ch nil)
      (fail state error/unexpected-eof))))

;; fparsec: anyOf, noneOf
;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Support a variant of match that takes a custom msgs fn
  #_(map error/expected chars)
  (match (set chars) (str "any of '" chars "'")))

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
        msgs     (error/expected-str s)
        eof-msgs (cons (first error/unexpected-eof) msgs)]
    (fn [state _ ok! fail _]
      (if (state/matches-str? state s)
        (ok! (state/skip state length) s nil)
        (if (state/at-end? state)
          (fail state eof-msgs)
          (fail state msgs))))))

(defn string-return [ch x]
  (p/>> (string ch) (p/return x)))

(defn string-ic [s]
  (check-string-literal s)
  (let [length   (count s)
        msgs     (error/expected-str s) ;; TODO: expected-str-ic?
        eof-msgs (cons (first error/unexpected-eof) msgs)]
    (fn [state _ ok! fail _]
      (if (state/matches-str-ic? state s)
        (ok! (state/skip state length)
             (state/peek-str state length)
             nil)
        (if (state/at-end? state)
          (fail state eof-msgs)
          (fail state msgs))))))

;; fparsec: restOfLine, skipRestOfLine
;; fparsec: charsTillString
#_(defn chars-till-str [s skip? max-count])

;; fparsec: manySatisfy
;; fparsec: manySatisfy2 -- different pred for first character
;; fparsec: many1Satisfy: one or more
;; fparsec: manyMinMaxSatisfy
#_(defn *match
    "Parses a sequence of zero or more characters satisfying `pred`,
   and returns them as a string."
    [pred])

;; fparsec: regex
;; fparsec: identifier

;; fparsec: combine parsers to return strings
;;   manyChars, manyStrings, skipped

;; fparsec: number parsers (int, float)
