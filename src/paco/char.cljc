(ns paco.char
  (:refer-clojure :exclude [char newline])
  (:require [paco.core :as p]
            [paco.detail.char-preds :as char-preds]
            [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.parsers :as dp]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.scanner :as scanner]))

;;---------------------------------------------------------
;; Character parsers

(defn newline-return [value]
  (let [expected-error (error/expected "newline")]
    (fn [scanner reply]
      (if-let [ch (scanner/peek scanner)]
        (if (= \newline ch)
          (do
            (scanner/skip! scanner)
            (reply/ok reply value))
          (if (= \return ch)
            (do
              (scanner/skip! scanner)
              (when (= \newline (scanner/peek scanner))
                (scanner/skip! scanner))
              (reply/ok reply value))
            (reply/fail reply (error/merge expected-error (error/unexpected-input ch)))))
        (reply/fail reply error/unexpected-end)))))

(def newline (newline-return \newline))

(def skip-newline (newline-return nil))

(defn- match-char [pred expected-error]
  (let [pred (char-preds/pred pred)]
    (fn [scanner reply]
      (let [ch (scanner/read-char-when! scanner pred)]
        (if ch
          (reply/ok reply ch)
          (reply/fail reply (error/merge expected-error
                                         (if (nil? ch)
                                           error/unexpected-end
                                           (error/unexpected-input (scanner/peek scanner))))))))))

;; fparsec: satisfy; normalises newlines
(defn match
  ([pred]
   (match pred nil))
  ([pred label]
   (match-char pred (when label (error/expected label)))))

;; fparsec: pchar
(defn char [ch]
  (match-char (char-preds/eq ch) (error/expected-input ch)))

(defn char-return [ch value]
  (p/return (char ch) value))

(defn skip-char [ch]
  ;; ? This is slower than `char` -- optimise?
  (char-return ch nil))

;; fparsec: + skipAnyChar
(def any-char p/any-token)

;; fparsec: + skip variants
(defn any-of [chars]
  (match-char (char-preds/among chars) (map error/expected-input (set chars))))

(defn none-of [chars]
  ;; TODO: error message: (expected "any char not in ...")
  (match-char (char-preds/not-among chars) nil))

(defn char-range
  ([min-ch max-ch]
   (char-range min-ch max-ch nil))
  ([min-ch max-ch label]
   (match (char-preds/in-range min-ch max-ch) label)))

(def ascii-upper
  (match char-preds/ascii-upper? "ASCII upper-case letter"))

(def ascii-lower
  (match char-preds/ascii-lower? "ASCII lower-case letter"))

(def ascii-letter
  (match char-preds/ascii-letter? "ASCII letter"))

(def upper
  (match char-preds/upper? "upper-case letter"))

(def lower
  (match char-preds/lower? "lower-case letter"))

(def letter
  (match char-preds/letter? "letter"))

(def digit
  (match char-preds/digit? "decimal digit"))

(def hex
  (match char-preds/hex? "hexadecimal digit"))

(def octal
  (match char-preds/octal? "octal digit"))

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
  (let [length (count s)
        expected-error (error/expected-input s)
        unexpected-end (error/merge error/unexpected-end expected-error)]
    (fn [scanner reply]
      (if (scanner/matches-str? scanner s)
        (do
          (scanner/untracked-skip! scanner length)
          (reply/ok reply s))
        (reply/fail reply (if (scanner/end? scanner)
                            unexpected-end
                            expected-error))))))

(defn string-return [ch x]
  (p/return (string ch) x))

(defn string-ci [s]
  (check-string-literal s)
  (let [length (count s)
        expected-error (error/expected-input s) ;; ? expected-input-ci?
        unexpected-end (error/merge error/unexpected-end expected-error)]
    (fn [scanner reply]
      (if (scanner/matches-str-ci? scanner s)
        (let [value (scanner/peek-str scanner length)]
          (scanner/untracked-skip! scanner length)
          (reply/ok reply value))
        (reply/fail reply (if (scanner/end? scanner)
                            unexpected-end
                            expected-error))))))

;; fparsec: restOfLine, skipRestOfLine
;; Can be implemented with regex
;; fparsec: charsTillString
#_(defn chars-until-str [s skip? max-count])

;; fparsec: manySatisfy
;; fparsec: manySatisfy2 -- different pred for first character
;; fparsec: many1Satisfy: one or more
;; fparsec: manyMinMaxSatisfy
(defn *match
  "Parses a sequence of zero or more characters satisfying `pred`,
   and returns them as a string."
  [pred]
  (let [pred (char-preds/pred pred)]
    (fn [scanner reply]
      (let [start (scanner/index scanner)
            value (when (pos? (scanner/skip-chars-while! scanner pred))
                    (scanner/read-from scanner start))]
        (reply/ok reply value)))))

(defn *skip-match [pred]
  (let [pred (char-preds/pred pred)]
    (fn [scanner reply]
      (scanner/skip-chars-while! scanner pred)
      (reply/ok reply nil))))

(defn- re-match-length [m]
  #?(:bb   (- (.end m) (.start m))
     :clj  (unchecked-subtract-int
            (.end ^java.util.regex.MatchResult m)
            (.start ^java.util.regex.MatchResult m))
     :cljs (.-length (aget m 0))))

;;? Add regex-groups?
(defn regex [re]
  (let [expected-error (error/expected (str "pattern" re))]
    (fn [scanner reply]
      (if-some [m (scanner/re-match scanner re)]
        (reply/ok reply (scanner/read-str! scanner (re-match-length m)))
        (reply/fail reply (error/merge expected-error
                                       (error/unexpected-token-or-end scanner)))))))

;; fparsec: identifier

;; fparsec: manyChars, manyStrings
(defn *str [p]
  (dp/repeat-many `*str p rfs/string))

;; fparsec: many1Chars, many1Strings
(defn +str [p]
  (dp/repeat-min `+str p rfs/string 1))

(defn strcat [& ps]
  (dp/pseq ps rfs/string))

(defn skipped [p]
  (reify parser/IParser
    (apply [_ scanner reply]
      (let [start (scanner/index scanner)
            reply (parser/apply p scanner reply)]
        (cond-> reply
          (reply/ok? reply) (reply/with-value (scanner/read-from scanner start)))))
    (children [_] [p])))

;; fparsec: number parsers (int, float)
