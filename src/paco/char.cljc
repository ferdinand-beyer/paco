(ns paco.char
  (:refer-clojure :exclude [char newline])
  (:require [paco.char.preds :as preds]
            [paco.core :as p]
            [paco.detail.advanced :as advanced]
            [paco.detail.error :as error]
            [paco.detail.reply :as reply]
            [paco.detail.rfs :as rfs]
            [paco.detail.source :as source]))

;;---------------------------------------------------------
;; Character source position

(defn position
  "This parser returns the current position in the input stream."
  [source reply]
  (reply/ok reply (source/position source)))

;; ? Use 1-based line/col?  0-based makes sense for programming,
;; ? but the position is probably mostly used for display to users.

(defn line-index
  "Returns the line index of the position `pos`.  The first line index is 0."
  [pos]
  (source/pos-line pos))

(defn column-index
  "Returns the column index of the position `pos`.  The first column index is 0."
  [pos]
  (source/pos-col pos))

;;---------------------------------------------------------
;; Character parsers

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
  "Returns a parser that succeeds if the next character in the input
   stream satisfies the predicate `pred`, and returns the accepted character.
   Otherwise, the parser fails without changing the parser state."
  ([pred]
   (satisfy pred nil))
  ([pred label]
   (satisfy-char pred (when label (error/expected label)))))

(defn char
  "Returns a parser that succeeds if `ch` comes next in the input stream and
   returns the character `ch`.

   Similar to:
   - fparsec: `pchar`"
  [ch]
  (satisfy-char (preds/eq ch) (error/expected-input ch)))

(defn char-return
  "Returns a parser that succeeds if `ch` comes next in the input stream,
   returning `value`."
  [ch value]
  (p/return (char ch) value))

(defn skip-char [ch]
  ;; ? This is slower than `char` -- optimise?
  (char-return ch nil))

(defn peek-char
  "Returns a parser that succeeds if the character `ch` comes next, without
   changing parser state."
  [ch]
  (let [expected (error/expected-input ch)]
    (fn [source reply]
      (let [next-ch (source/peek source)]
        (if (= ch next-ch)
          (reply/ok reply ch)
          (reply/fail reply (error/merge expected (if (nil? next-ch)
                                                    error/unexpected-end
                                                    (error/unexpected-input next-ch)))))))))

;; fparsec: + nextCharSatisfies, nextCharSatisfiesNot

(def any-char p/any-token)
(def skip-any-char p/skip-any-token)

;; fparsec: + skip variants
(defn any-of
  "Returns a parser that succeeds if the next character in the input stream
   is contained in the string `s`."
  [s]
  (satisfy-char (preds/any-of s) (map error/expected-input (set s))))

(defn none-of
  "Returns a parser that succeeds if the next character in the input stream
   is not contained in the string `s`."
  [s]
  ;; TODO: error message: (expected "any char not in ...")
  (satisfy-char (preds/none-of s) nil))

(defn char-range
  "Returns a parser that succeeds if the next character in the
   input stream is within the range `[min-ch max-ch]` (inclusive).

   Example: `(char-range \\a \\z)` matches a lower-case ASCII character."
  ([min-ch max-ch]
   (char-range min-ch max-ch nil))
  ([min-ch max-ch label]
   (satisfy (preds/in-range min-ch max-ch) label)))

(defn newline-return
  "Returns a parser that matches a newline sequence and returns `x`."
  [value]
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

(def newline
  "Parses a common newline sequence: `\\n`, `\\r`,
   or `\\r\\n`, normalizing the return value to `\\n`."
  (newline-return \newline))

(def skip-newline (newline-return nil))

(def unicode-newline
  "Parses any Unicode newline sequence and returns `\\n`."
  (p/alt newline (p/return (any-of "\u0085\u2028\u2029") \newline)))

(def tab
  "This parser matches a tabulator character."
  (char \tab))

(def space
  "This parser matches a common whitespace character: space, tabulator,
   newline or carriage return."
  (satisfy preds/space? "whitespace character"))

(def unicode-space
  "This parser matches any Unicode whitespace character."
  (satisfy preds/unicode-space? "whitespace character"))

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

;;---------------------------------------------------------
;; String parsers

(defn- check-string-literal [s]
  (when (empty? s)
    (throw (ex-info "String literal cannot be empty" {})))
  (when (re-find #"[\r\n]" s)
    (throw (ex-info "String literal cannot contain newlines" {}))))

(defn string
  "Returns a parser that succeeds if the string `s` comes next in the input
   stream.

   This parser is atomic: it does not change the input state when only a prefix
   of `s` matches.

   As an optimisation, this parser skips line tracking, therefore `s` cannot
   contain newlines.

   Similar to:
   - fparsec: `pstring`"
  [s]
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

(defn string-return
  "Like `(return (string s) x)`."
  [s x]
  (p/return (string s) x))

(defn string-ci
  "Like `string`, but matches `s` case-insensitive."
  [s]
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

;; fparsec: + skipString, CI variants,
;;   anyString, skipAnyString

;; fparsec: restOfLine, skipRestOfLine
;; Can be implemented with regex

;; fparsec: charsTillString
#_(defn chars-until-str [s skip? max-count])

(defn *str-until
  "Returns a parser that repeatedly applies the parser `p` for as long
   as `pend` fails (without changing the parser state).  It returns a string
   concatenation of the results returned by `p`.

   When `include-end?` is true, also includes the result of `pend` in the
   returned collection.

   Similar to:
   - fparsec: `manyCharsTill`"
  ([p pend]
   (*str-until p pend false))
  ([p pend include-end?]
   (advanced/until `*str-until p pend rfs/string true include-end?)))

(defn +str-until
  "Like `*str-until`, but requires `p` to succeed at least once.

   Similar to:
   - fparsec: `many1CharsTill`"
  ([p pend]
   (+str-until p pend false))
  ([p pend include-end?]
   (advanced/until `+str-until p pend rfs/string false include-end?)))

(defn *satisfy
  "Returns a parser that parses a sequence of zero or more characters
   satisfying `pred`, and returns them as a string.

   Similar to:
   - fparsec: `manySatisfy`"
  [pred]
  (let [pred (preds/pred pred)]
    (fn [source reply]
      (source/with-resource [mark (source/mark source)]
        (let [value (when (pos? (source/skip-chars-while! source pred))
                      (source/read-from source mark))]
          (reply/ok reply value))))))

(defn +satisfy
  "Like `*satisfy`, but requires `pred` to succeed at least once.

   Similar to:
   - fparsec: `many1Satisfy`"
  [pred]
  (let [pred (preds/pred pred)]
    (fn [source reply]
      (source/with-resource [mark (source/mark source)]
        (if (pos? (source/skip-chars-while! source pred))
          (reply/ok reply (source/read-from source mark))
          (reply/fail reply (error/unexpected-token-or-end source)))))))

;; fparsec: manySatisfy2 -- different pred for first character
;; useful for identifiers

;; fparsec: manyMinMaxSatisfy

(defn *skip-satisfy
  "Like `*satisfy`, but returns `nil`."
  [pred]
  (let [pred (preds/pred pred)]
    (fn [source reply]
      (source/skip-chars-while! source pred)
      (reply/ok reply nil))))

(defn +skip-satisfy
  "Like `+satisfy`, but returns `nil`."
  ([pred]
   (+skip-satisfy pred nil))
  ([pred label]
   (let [unexpected (when label (error/unexpected label))]
     (fn [source reply]
       (if (pos? (source/skip-chars-while! source pred))
         (reply/ok reply nil)
         (reply/fail reply unexpected))))))

(def *skip-space
  "This parser skips over zero or more space characters.

   Similar to:
   - fparsec: `spaces`"
  (*skip-satisfy preds/space?))

(def +skip-space
  "This parser skips over one or more space characters.

   Similar to:
   - fparsec: `spaces1`"
  (+skip-satisfy preds/space? "whitespace characters"))

(defn- re-match-length [m]
  #?(:bb   (- (.end m) (.start m))
     :clj  (unchecked-subtract-int
            (.end ^java.util.regex.MatchResult m)
            (.start ^java.util.regex.MatchResult m))
     :cljs (.-length (aget m 0))))

(defn- regex-parser [re label result-fn]
  (let [expected-error (error/expected label)]
    (fn [source reply]
      (if-some [m (source/re-match source re)]
        (let [n (re-match-length m)]
          (reply/ok reply (result-fn source n m)))
        (reply/fail reply (error/merge expected-error
                                       (error/unexpected-token-or-end source)))))))

(defn- re-label [re]
  (str "pattern '" #?(:clj re, :cljs (.-source re)) "'"))

(defn regex
  "Returns a parser that matches the regular expression pattern `re` at the
   current source position and returns the matched string."
  ([re]
   (regex re (re-label re)))
  ([re label]
   (regex-parser re label (fn [source n _] (source/read-str! source n)))))

(defn regex-groups
  "Returns a parser that matches the regular expression pattern `re`
   at the current source position and returns the matched string or a vector of
   groups like `re-groups`."
  ([re]
   (regex-groups re (re-label re)))
  ([re label]
   (regex-parser re label (fn [source n m]
                            (source/skip! source n)
                            #?(:clj  (re-groups m)
                               :cljs (if (== (.-length m) 1)
                                       (aget m 0)
                                       (vec m)))))))

;; fparsec: identifier

(defn *str
  "Returns a parser that matches `p` zero or more times and returns a string of
   the concatenated results of `p`.

   Similar to:
   - fparsec: `manyChars`, `manyStrings`"
  [p]
  (advanced/repeat-many `*str p rfs/string))

(defn +str
  "Like `*str`, but requires `p` to match at least once.

   Similar to:
   - fparsec: `many1Chars`, `many1Strings`"
  [p]
  (advanced/repeat-min `+str p rfs/string 1))

(defn strcat
  "Returns a parser that applies the parsers `ps` in sequence and returns
   the string concatenation of their return values"
  [& ps]
  (advanced/sequence rfs/string ps))

(defn skipped
  "Returns a parser that applies the parser `p` and returns the skipped source
   characters as a string (ignoring the result of `p`)."
  [p]
  (fn [source reply]
    (source/with-resource [mark (source/mark source)]
      (let [reply (p source reply)]
        (cond-> reply
          (reply/ok? reply) (reply/with-value (source/read-from source mark)))))))

;; fparsec: number parsers (int, float)
;; fparsec: assertions: notFollowedByEof, followedByNewline, notFollowedByNewline
;;  followedByString, etc.
