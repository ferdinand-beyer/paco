(ns comparse.char
  (:refer-clojure :exclude [char])
  (:require [comparse.error :as error]
            [comparse.state :as state]))

;;---------------------------------------------------------
;; Characters

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
       (if-let [token (state/peek state)]
         (if (pred token)
           (ok! (state/skip-char state) token nil)
           (fail state (wrap-msgs (error/unexpected token))))
         (fail state (wrap-msgs error/unexpected-eof)))))))

;; fparsec: pchar
;; fparsec: + skipChar, charReturn
(defn char [c]
  (match #(= c %) c))

;; fparsec: + skipAnyChar
(def any-char
  (fn [state _ ok! fail _]
    (if-let [c (state/peek state)]
      (ok! (state/skip-char state) c nil)
      (fail state error/unexpected-eof))))

;; fparsec: anyOf, noneOf
;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Support a variant of satisfy that takes a custom msgs fn
  #_(map error/expected chars)
  (match (set chars) (str "any of '" chars "'")))

;; fparsec: asciiLower, asciiUpper, asciiLetter
;; fparsec: lower, upper, letter
;; fparsec: digit, hex, octal

;; fparsec: tab
;; fparsec: newline, skipNewline, newlineReturn + unicode variants
;; fparsec: spaces, spaces1 + unicode variants

;;---------------------------------------------------------
;; strings

(defn- string' [s match]
  {:pre [(seq s)
           ;; So that we can skip without tracking lines.
         (not (re-find #"[\r\n]" s))]}
  (let [length   (count s)
        msgs     (error/expected-str s)
        eof-msgs (cons (first error/unexpected-eof) msgs)]
    (fn [state _ ok! fail _]
      (if (match state s)
        (ok! (state/skip state length) s nil)
        (if (state/at-end? state)
          (fail state eof-msgs)
          (fail state msgs))))))

;; fparsec: pstring
;; fparsec: + skipString, stringReturn, CI variants,
;;   anyString, skipAnyString
(defn string [s]
  (string' s state/matches-str?))

(defn string-ci [s]
  (string' s state/matches-str-ic?))

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
