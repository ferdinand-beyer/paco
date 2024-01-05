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

(defn- match-char [pred skip expected-error]
  (fn [scanner reply]
    (if-let [ch (scanner/peek scanner)]
      (if (pred ch)
        (do
          (skip scanner)
          (reply/ok reply ch))
        (reply/fail reply (error/merge expected-error (error/unexpected-input ch))))
      (reply/fail reply (error/merge expected-error error/unexpected-end)))))

;; fparsec: satisfy; normalises newlines
(defn match
  ([pred]
   (match pred nil))
  ([pred label]
   (match-char pred
               (if (or (pred \newline) (pred \return))
                 scanner/skip!
                 scanner/untracked-skip!)
               (when label
                 (error/expected label)))))

(defn char-return [ch value]
  (let [skip (if (or (= \newline ch) (= \return ch))
               scanner/skip!
               scanner/untracked-skip!)
        expected-error (error/expected-input ch)]
    (fn [scanner reply]
      (if-let [ch* (scanner/peek scanner)]
        (if (= ch ch*)
          (do
            (skip scanner)
            (reply/ok reply value))
          (reply/fail reply (error/merge expected-error (error/unexpected-input ch*))))
        (reply/fail reply (error/merge expected-error error/unexpected-end))))))

;; fparsec: pchar
(defn char [ch]
  (char-return ch ch))

(defn skip-char [ch]
  (char-return ch nil))

;; fparsec: + skipAnyChar
(def any-char p/any-token)

;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Optimise for newlines?
  (let [cset (set chars)
        expected-error (map error/expected-input cset)]
    (match-char cset
                (if (or (cset \newline) (cset \return))
                  scanner/skip!
                  scanner/untracked-skip!)
                expected-error)))

(defn none-of [chars]
  ;; TODO: (expected "any char not in ...")
  (let [cset (set chars)]
    (match-char (complement cset)
                (if (and (cset \newline) (cset \return))
                  scanner/untracked-skip!
                  scanner/skip!)
                nil)))

(defn char-range
  ([min-ch max-ch]
   (char-range min-ch max-ch nil))
  ([min-ch max-ch label]
   (let [min-cp (char-preds/code-point min-ch)
         max-cp (char-preds/code-point max-ch)]
     (match #(<= min-cp (char-preds/code-point %) max-cp) label))))

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

;; fparsec: manyChars, manyStrings
(defn str* [p]
  (dp/repeat-many `str* p rfs/string))

;; fparsec: many1Chars, many1Strings
(defn str+ [p]
  (dp/repeat-min `str+ p rfs/string 1))

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
