(ns paco.char
  (:refer-clojure :exclude [char newline])
  (:require [paco.char.preds :as preds]
            [paco.core :as p]
            [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.parsers :as dp]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.source :as source]))

;;---------------------------------------------------------
;; Character parsers

(defn newline-return [value]
  (let [expected-error (error/expected "newline")]
    (fn [source reply]
      (if-let [ch (source/peek source)]
        (if (= \newline ch)
          (do
            (source/skip! source)
            (reply/ok reply value))
          (if (= \return ch)
            (do
              (source/skip! source)
              (when (= \newline (source/peek source))
                (source/skip! source))
              (reply/ok reply value))
            (reply/fail reply (error/merge expected-error (error/unexpected-input ch)))))
        (reply/fail reply error/unexpected-end)))))

(def newline (newline-return \newline))

(def skip-newline (newline-return nil))

(defn- satisfy-char [pred expected-error]
  (let [pred (preds/pred pred)]
    (fn [source reply]
      (let [ch (source/read-char-when! source pred)]
        (if ch
          (reply/ok reply ch)
          (reply/fail reply (error/merge expected-error
                                         (if (nil? ch)
                                           error/unexpected-end
                                           (error/unexpected-input (source/peek source))))))))))

;; fparsec: normalises newlines
(defn satisfy
  ([pred]
   (satisfy pred nil))
  ([pred label]
   (satisfy-char pred (when label (error/expected label)))))

;; fparsec: pchar
(defn char [ch]
  (satisfy-char (preds/eq ch) (error/expected-input ch)))

(defn char-return [ch value]
  (p/return (char ch) value))

(defn skip-char [ch]
  ;; ? This is slower than `char` -- optimise?
  (char-return ch nil))

(def any-char p/any-token)
(def skip-any-char p/skip-any-token)

;; fparsec: + skip variants
(defn any-of [chars]
  (satisfy-char (preds/among chars) (map error/expected-input (set chars))))

(defn none-of [chars]
  ;; TODO: error message: (expected "any char not in ...")
  (satisfy-char (preds/not-among chars) nil))

(defn char-range
  ([min-ch max-ch]
   (char-range min-ch max-ch nil))
  ([min-ch max-ch label]
   (satisfy (preds/in-range min-ch max-ch) label)))

(def ascii-upper
  (satisfy preds/ascii-upper? "ASCII upper-case letter"))

(def ascii-lower
  (satisfy preds/ascii-lower? "ASCII lower-case letter"))

(def ascii-letter
  (satisfy preds/ascii-letter? "ASCII letter"))

(def upper
  "Parses an upper-case letter character."
  (satisfy preds/upper? "upper-case letter"))

(def lower
  "Parses a lower-case letter character."
  (satisfy preds/lower? "lower-case letter"))

(def letter
  "Parses a letter character."
  (satisfy preds/letter? "letter"))

(def digit
  "Parses a decimal digit."
  (satisfy preds/digit? "decimal digit"))

(def hex
  "Parses a hexadecimal digit."
  (satisfy preds/hex? "hexadecimal digit"))

(def octal
  "Parses an octal digit."
  (satisfy preds/octal? "octal digit"))

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
    (fn [source reply]
      (if (source/matches-str? source s)
        (do
          (source/untracked-skip! source length)
          (reply/ok reply s))
        (reply/fail reply (if (source/end? source)
                            unexpected-end
                            expected-error))))))

(defn string-return [ch x]
  (p/return (string ch) x))

(defn string-ci [s]
  (check-string-literal s)
  (let [length (count s)
        expected-error (error/expected-input s) ;; ? expected-input-ci?
        unexpected-end (error/merge error/unexpected-end expected-error)]
    (fn [source reply]
      (if (source/matches-str-ci? source s)
        (let [value (source/peek-str source length)]
          (source/untracked-skip! source length)
          (reply/ok reply value))
        (reply/fail reply (if (source/end? source)
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
(defn *satisfy
  "Parses a sequence of zero or more characters satisfying `pred`,
   and returns them as a string."
  [pred]
  (let [pred (preds/pred pred)]
    (fn [source reply]
      (let [start (source/index source)
            value (when (pos? (source/skip-chars-while! source pred))
                    (source/read-from source start))]
        (reply/ok reply value)))))

(defn *skip-satisfy [pred]
  (let [pred (preds/pred pred)]
    (fn [source reply]
      (source/skip-chars-while! source pred)
      (reply/ok reply nil))))

(defn- re-match-length [m]
  #?(:bb   (- (.end m) (.start m))
     :clj  (unchecked-subtract-int
            (.end ^java.util.regex.MatchResult m)
            (.start ^java.util.regex.MatchResult m))
     :cljs (.-length (aget m 0))))

;;? Add regex-groups?
(defn regex
  "Returns a parser that matches the regular expression pattern `re` at the
   current source position and returns matched string."
  ([re]
   (regex re (str "pattern '" #?(:clj re, :cljs (.-source re)) "'")))
  ([re label]
   (let [expected-error (error/expected label)]
     (fn [source reply]
       (if-some [m (source/re-match source re)]
         (reply/ok reply (source/read-str! source (re-match-length m)))
         (reply/fail reply (error/merge expected-error
                                        (error/unexpected-token-or-end source))))))))

;; fparsec: identifier

;; fparsec: manyChars, manyStrings
(defn *str [p]
  (dp/repeat-many `*str p rfs/string))

;; fparsec: many1Chars, many1Strings
(defn +str [p]
  (dp/repeat-min `+str p rfs/string 1))

(defn strcat [& ps]
  (dp/reduce ps rfs/string))

(defn skipped [p]
  (reify parser/IParser
    (apply [_ source reply]
      (source/with-release [mark (source/mark source)]
        (let [reply (parser/apply p source reply)]
          (cond-> reply
            (reply/ok? reply) (reply/with-value (source/read-from source mark))))))
    (children [_] [p])))

;; fparsec: number parsers (int, float)
