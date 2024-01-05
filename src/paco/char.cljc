(ns paco.char
  (:refer-clojure :exclude [char newline])
  (:require [paco.core :as p]
            [paco.detail :as detail]
            [paco.detail.char-preds :as char-preds]
            [paco.detail.error :as error]
            [paco.state :as state]))

;;---------------------------------------------------------
;; Character parsers

(defn newline-return [value]
  (fn [state reply]
    (if-let [ch (state/peek state)]
      (if (= \newline ch)
        (reply :ok (state/skip state) value nil)
        (if (= \return ch)
          (let [state (state/skip state)]
            (reply :ok (if (= \newline (state/peek state))
                         (state/skip state)
                         state)
                   value nil))
          (reply :error state nil (error/merge (error/expected "newline")
                                               (error/unexpected-input ch)))))
      (reply :error state nil error/unexpected-end))))

(def newline (newline-return \newline))

(def skip-newline (newline-return nil))

(defn- match-char [pred skip error]
  (fn [state reply]
    (if-let [ch (state/peek state)]
      (if (pred ch)
        (reply :ok (skip state) ch nil)
        (reply :error state nil (error (error/unexpected-input ch))))
      (reply :error state nil (error error/unexpected-end)))))

;; fparsec: satisfy; normalises newlines
(defn match
  ([pred]
   (match pred nil))
  ([pred label]
   (match-char pred
               (if (or (pred \newline) (pred \return))
                 state/skip
                 state/untracked-skip)
               (if label
                 (let [error (error/expected label)]
                   #(error/merge error %))
                 identity))))

(defn char-return [ch value]
  (let [skip  (if (or (= \newline ch) (= \return ch))
                state/skip
                state/untracked-skip)
        error (error/expected-input ch)]
    (fn [state reply]
      (if-let [next-ch (state/peek state)]
        (if (= ch next-ch)
          (reply :ok (skip state) value nil)
          (reply :error state nil (error/merge (error/unexpected-input next-ch) error)))
        (reply :error state nil (error/merge error/unexpected-end error))))))

;; fparsec: pchar
(defn char [ch]
  (char-return ch ch))

(defn skip-char [ch]
  (char-return ch nil))

;; fparsec: + skipAnyChar
(def any-char
  (fn [state reply]
    (if-let [ch (state/peek state)]
      (reply :ok (state/skip state) ch nil)
      (reply :error state nil error/unexpected-end))))

;; fparsec: + skip variants
(defn any-of [chars]
  ;; TODO: Optimise for newlines?
  (let [charset (set chars)
        error   (map error/expected-input charset)]
    (match-char charset
                (if (or (charset \newline) (charset \return))
                  state/skip
                  state/untracked-skip)
                #(error/merge error %))))

(defn none-of [chars]
  ;; TODO: (expected "any char not in ...")
  (let [charset (set chars)]
    (match-char (complement charset)
                (if (and (charset \newline) (charset \return))
                  state/untracked-skip
                  state/skip)
                identity)))

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
  (let [length   (count s)
        error     (error/expected-input s)
        error-end (error/merge error/unexpected-end error)]
    (fn [state reply]
      (if (state/matches-str? state s)
        (reply :ok (state/untracked-skip state length) s nil)
        (reply :error state nil (if (state/at-end? state)
                                  error-end
                                  error))))))

(defn string-return [ch x]
  (p/return (string ch) x))

(defn string-i [s]
  (check-string-literal s)
  (let [length    (count s)
        error     (error/expected-input s) ;; TODO: expected-str-ic?
        error-end (error/merge error/unexpected-end error)]
    (fn [state reply]
      (if (state/matches-str-i? state s)
        (reply :ok (state/untracked-skip state length)
               (state/peek-str state length) nil)
        (reply :error state nil (if (state/at-end? state)
                                  error-end
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

;; fparsec: manyChars, manyStrings
(defn str* [p]
  (detail/reduce-repeat `str* p detail/string-rf 0))

;; fparsec: many1Chars, many1Strings
(defn str+ [p]
  (detail/reduce-repeat `str+ p detail/string-rf 1))

(defn strcat [& ps]
  (detail/reduce-sequence detail/string-rf ps))

(defn skipped [p]
  (fn [state reply]
    (let [index0 (state/index state)]
      (detail/call p state (fn [status1 state1 value1 error1]
                             (if (detail/ok? status1)
                               (let [n (- (state/index state1) index0)]
                                 (reply :ok state1 (state/peek-str state n) error1))
                               (reply status1 state1 value1 error1)))))))

;; fparsec: number parsers (int, float)
