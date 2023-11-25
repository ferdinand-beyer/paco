(ns comparse.char
  (:refer-clojure :exclude [char])
  (:require [comparse.core :as p]
            [comparse.error :as error]
            [comparse.state :as state]))

;;---------------------------------------------------------
;; Characters

;; fparsec: pchar
;; fparsec: + skipChar, charReturn
(defn char [c]
  (p/satisfy #(= c %) c))

;; fparsec: + skipAnyChar
(def any-char
  (fn [state _ ok! fail _]
    (if-let [c (state/peek-char state)]
      (ok! (state/skip-char state) c nil)
      (fail state error/unexpected-eof))))

;; fparsec: anyOf, noneOf
;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Support a variant of satisfy that takes a custom msgs fn
  #_(map error/expected chars)
  (p/satisfy (set chars) (str "any of '" chars "'")))

;; fparsec: asciiLower, asciiUpper, asciiLetter
;; fparsec: lower, upper, letter
;; fparsec: digit, hex, octal

;; fparsec: tab
;; fparsec: newline, skipNewline, newlineReturn + unicode variants
;; fparsec: spaces, spaces1 + unicode variants

;;---------------------------------------------------------
;; strings

;; fparsec: pstring
;; fparsec: + skipString, stringReturn, CI variants,
;;   anyString, skipAnyString
(defn string [s]
  {:pre [(seq s)
         (not (re-find #"[\r\n]" s))]}
  (let [length       (count s)
        msgs (error/expected-str s)
        eof-msgs (cons (first error/unexpected-eof) msgs)]
    (fn [state _ ok! fail _]
      (if (state/matches? state s)
        (ok! (state/skip* state length) s nil)
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
