(ns paco.detail.source
  (:refer-clojure :exclude [peek re-groups])
  (:require #?(:bb      [paco.detail.source.impl :as impl]
               :clj     [clojure.core]
               :default [paco.detail.source.impl :as impl]))
  #?@(:bb   []
      :clj  [(:import [paco.detail.jvm ICharPredicate Source])]
      :cljs [(:require-macros paco.detail.source)]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (defn- tag
          ([x] (tag x 'paco.detail.jvm.Source))
          ([x tag]
           (vary-meta x assoc :tag tag))))

(defn release!
  "Releases any resources held by `x`."
  [x]
  #?(:bb      (impl/release! x)
     :clj     (.close ^java.lang.AutoCloseable x)
     :default (impl/release! x)))

(defmacro with-resource
  "Similar to `with-open`, but works in ClojureScript and SCI."
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [bindings & body]
  {:pre [(vector? bindings)
         (= 2 (count bindings))
         (symbol? (first bindings))]}
  `(let [~@bindings]
     (try
       ~@body
       (finally
         (release! ~(first bindings))))))

;; ISource

(defn index
  "Returns the current index in the `source`."
  #?(:clj {:inline (fn [source] `(.index ~(tag source)))})
  [source]
  #?(:bb      (impl/index source)
     :clj     (.index ^Source source)
     :default (impl/index source)))

(defn end?
  "Returns true if `source` is at the end (drained)."
  #?(:clj {:inline (fn [source] `(.atEnd ~(tag source)))})
  [source]
  #?(:bb      (impl/end? source)
     :clj     (.atEnd ^Source source)
     :default (impl/end? source)))

(defn peek
  "Returns the next token in the `source`, without consuming it.
   Returns `nil` if the source is at the end."
  #?(:clj {:inline (fn [source] `(.peek ~(tag source)))})
  [source]
  #?(:bb      (impl/peek source)
     :clj     (.peek ^Source source)
     :default (impl/peek source)))

(defn skip!
  "Skips the next token, or the next `n` tokens, and advances the source.
   Returns the number of tokens skipped."
  #?(:clj {:inline (fn
                     ([source]
                      `(.skip ~(tag source)))
                     ([source n]
                      `(.skip ~(tag source) ~n)))})
  ([source]
   #?(:bb      (impl/skip! source)
      :clj     (.skip ^Source source)
      :default (impl/skip! source)))
  ([source n]
   #?(:bb      (impl/skip! source n)
      :clj     (.skip ^Source source n)
      :default (impl/skip! source n))))

(defn mark
  "Marks the current source position, returning a mark object.  The source
   can subsequently be reset to this mark.  The caller is responsible to
   release the mark once it is no longer needed, preferrably using the
   [[with-resource]] macro."
  #?(:clj {:inline (fn [source] `(.mark ~(tag source)))})
  [source]
  #?(:bb      (impl/mark source)
     :clj     (.mark ^Source source)
     :default (impl/mark source)))

(defn at?
  "Returns true if the source is in the state represented by `mark`."
  #?(:clj {:inline (fn [source mark] `(.atMark ~(tag source) ~mark))})
  [source mark]
  #?(:bb      (impl/at? source mark)
     :clj     (.atMark ^Source source mark)
     :default (impl/at? source mark)))

(defn backtrack!
  "Restores the source to the state represented by `mark`."
  #?(:clj {:inline (fn [source mark] `(.backtrack ~(tag source) ~mark))})
  [source mark]
  #?(:bb      (impl/backtrack! source mark)
     :clj     (.backtrack ^Source source mark)
     :default (impl/backtrack! source mark)))

;; ICharSource

(defn peek-str
  "Returns a string of up to `n` characters at the current source position,
   without consuming them."
  #?(:clj {:inline (fn [source n] `(.peekString ~(tag source) ~n))})
  [source n]
  #?(:bb      (impl/peek-str source n)
     :clj     (.peekString ^Source source n)
     :default (impl/peek-str source n)))

(defn read-str!
  "Returns a string of up to `n` characters and advances the source position.
   Returns the number of characters read."
  #?(:clj {:inline (fn [source n] `(.readString ~(tag source) ~n))})
  [source n]
  #?(:bb      (impl/read-str! source n)
     :clj     (.readString ^Source source n)
     :default (impl/read-str! source n)))

(defn satisfies-char-pred?
  "Returns true if the next character in the `source` satisfies the char
   predicate `pred`."
  #?(:clj {:inline (fn [source pred]
                     `(.satisfies ~(tag source) ~(tag pred 'paco.detail.jvm.ICharPredicate)))})
  [source pred]
  #?(:bb      (impl/satisfies-char-pred? source pred)
     :clj     (.satisfies ^Source source ^ICharPredicate pred)
     :default (impl/satisfies-char-pred? source pred)))

(defn matches-str?
  "Returns true if the next characters in `source` are equal to the ones in
   string `s`."
  #?(:clj {:inline (fn [source s] `(.matchesString ~(tag source) ~s))})
  [source s]
  #?(:bb      (impl/matches-str? source s)
     :clj     (.matchesString ^Source source s)
     :default (impl/matches-str? source s)))

(defn matches-str-ci?
  "Like `matches-str?`, but case-insensitive."
  #?(:clj {:inline (fn [source s] `(.matchesStringIgnoreCase ~(tag source) ~s))})
  [source s]
  #?(:bb      (impl/matches-str-ci? source s)
     :clj     (.matchesStringIgnoreCase ^Source source s)
     :default (impl/matches-str-ci? source s)))

(defn re-match
  "Matches the regular expression `re` at the current source position and
   returns a platform-dependent match result object."
  #?(:clj {:inline (fn [source re] `(.matchRegex ~(tag source) ~re))})
  [source re]
  #?(:bb      (impl/re-match source re)
     :clj     (.matchRegex ^Source source re)
     :default (impl/re-match source re)))

(defn re-groups
  "Matches the regular expression `re` at the current source position and
   returns the matched string or a vector of captured groups,
   like `clojure.core/re-groups`."
  [source re]
  (when-let [m (re-match source re)]
    #?(:clj  (clojure.core/re-groups m)
       :cljs (if (== (.-length m) 1)
               (aget m 0)
               (vec m)))))

(defn read-char-when!
  "Reads and returns the next character when it satisfies `pred`.  Returns
   `false` when it does not satsify `pred`, and `nil` at the end of the
   source."
  [source pred]
  #?(:bb      (impl/read-char-when! source pred)
     :clj     (let [ch (.readCharWhen ^Source source ^ICharPredicate pred)]
                (case ch -1 nil -2 false (char ch)))
     :default (impl/read-char-when! source pred)))

(defn skip-chars-while!
  "Skips the next chars in `source` as long as they satisfy the char
   predicate `pred`.  Returns the number of skipped characters."
  #?(:clj {:inline (fn [source pred]
                     `(.skipCharsWhile ~(tag source) ~(tag pred 'paco.detail.jvm.ICharPredicate)))})
  [source pred]
  #?(:bb      (impl/skip-chars-while! source pred)
     :clj     (.skipCharsWhile ^Source source ^ICharPredicate pred)
     :default (impl/skip-chars-while! source pred)))

(defn read-from
  "Returns a string of all characters from the marked position (inclusive)
   to the current position (exclusive)."
  #?(:clj {:inline (fn [source mark] `(.readFrom ~(tag source) ~mark))})
  [source mark]
  #?(:bb      (impl/read-from source mark)
     :clj     (.readFrom ^Source source mark)
     :default (impl/read-from source mark)))

;; IUserStateSource

(defn modcount
  "Returns an integer representing the number of times the `source` has been
   modified.  This can be used to efficiently determine if the parser state
   has been modified."
  #?(:clj {:inline (fn [source] `(.modCount ~(tag source)))})
  [source]
  #?(:bb      (impl/modcount source)
     :clj     (.modCount ^Source source)
     :default (impl/modcount source)))

(defn backtrack-modified!
  "Like `backtrack!`, but does not reset the `modcount`."
  #?(:clj {:inline (fn [source state] `(.backtrackModified ~(tag source) ~state))})
  [source state]
  #?(:bb      (impl/backtrack-modified! source state)
     :clj     (.backtrackModified ^Source source state)
     :default (impl/backtrack-modified! source state)))

(defn user-state
  "Returns the current user state."
  #?(:clj {:inline (fn [source] `(.getUserState ~(tag source)))})
  [source]
  #?(:bb      (impl/user-state source)
     :clj     (.getUserState ^Source source)
     :default (impl/user-state source)))

(defn with-user-state!
  "Sets the user state to `state`, and returns the `source`."
  #?(:clj {:inline (fn [source state] `(doto ~(tag source) (.setUserState ~state)))})
  [source state]
  #?(:bb      (impl/with-user-state! source state)
     :clj     (doto ^Source source (.setUserState state))
     :default (impl/with-user-state! source state)))

;; ILineTrackingSource

(defn position
  "Returns the position (line and column indexes) of the `source`, at
   the current or a previous `index`."
  #?(:clj {:inline (fn
                     ([source]
                      `(.position ~(tag source)))
                     ([source index]
                      `(.position ~(tag source) ~index)))})
  ([source]
   #?(:bb      (impl/position source)
      :clj     (.position ^Source source)
      :default (impl/position source)))
  ([source index]
   #?(:bb      (impl/position source index)
      :clj     (.position ^Source source index)
      :default (impl/position source index))))

(defn pos-line
  "Returns the line index of the position `pos`.  The first line index is 0."
  #?(:clj {:inline (fn [pos] `(bit-shift-right ~pos 32))})
  [pos]
  #?(:bb      (:line pos)
     :clj     (bit-shift-right pos 32)
     :default (.-line ^impl/Position pos)))

(defn pos-col
  "Returns the column index of the position `pos`.  The first column index is 0."
  #?(:clj {:inline (fn [pos] `(bit-and ~pos 0xffffffff))})
  [pos]
  #?(:bb      (:col pos)
     :clj     (bit-and pos 0xffffffff)
     :default (.-col ^impl/Position pos)))

(defn untracked-skip!
  "Like `skip!`, but does not track line numbers.  For optimisation, when we
   have already determined that the next character is not a line terminator."
  #?(:clj {:inline (fn
                     ([source]
                      `(.untrackedSkip ~(tag source)))
                     ([source n]
                      `(.untrackedSkip ~(tag source) ~n)))})
  ([source]
   #?(:bb      (impl/untracked-skip! source)
      :clj     (.untrackedSkip ^Source source)
      :default (impl/untracked-skip! source)))
  ([source n]
   #?(:bb      (impl/untracked-skip! source n)
      :clj     (.untrackedSkip ^Source source n)
      :default (impl/untracked-skip! source n))))

(defn of
  "Creates a source from `input`."
  ([input]
   (of input nil))
  ([input opts]
   #?(:bb      (impl/of input opts)
      :clj     (Source/of ^String input (:user-state opts) (boolean (:line-tracking? opts true)))
      :default (impl/of input opts))))
