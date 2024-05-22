(ns paco.detail.source
  (:refer-clojure :exclude [peek re-groups])
  (:require #?(:bb      [paco.detail.source.impl :as impl]
               :clj     [clojure.core]
               :default [paco.detail.source.impl :as impl])
            #?(:clj [paco.detail.position :as pos]))
  #?@(:bb   []
      :clj  [(:import [paco.detail.jvm ICharPredicate Source])]
      :cljs [(:require-macros paco.detail.source)]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (defn- tag
          ([x] (tag x 'paco.detail.jvm.Source))
          ([x tag]
           (vary-meta x assoc :tag tag))))

(defn release! [x]
  #?(:bb      (impl/release! x)
     :clj     (.close ^java.lang.AutoCloseable x)
     :default (impl/release! x)))

(defmacro with-release [bindings & body]
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
  #?(:clj {:inline (fn [source] `(.index ~(tag source)))})
  [source]
  #?(:bb      (impl/index source)
     :clj     (.index ^Source source)
     :default (impl/index source)))

(defn end?
  #?(:clj {:inline (fn [source] `(.atEnd ~(tag source)))})
  [source]
  #?(:bb      (impl/end? source)
     :clj     (.atEnd ^Source source)
     :default (impl/end? source)))

(defn peek
  #?(:clj {:inline (fn [source] `(.peek ~(tag source)))})
  [source]
  #?(:bb      (impl/peek source)
     :clj     (.peek ^Source source)
     :default (impl/peek source)))

(defn skip!
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
  #?(:clj {:inline (fn [source] `(.mark ~(tag source)))})
  [source]
  #?(:bb      (impl/mark source)
     :clj     (.mark ^Source source)
     :default (impl/mark source)))

(defn at?
  #?(:clj {:inline (fn [source mark] `(.atMark ~(tag source) ~mark))})
  [source mark]
  #?(:bb      (impl/at? source mark)
     :clj     (.atMark ^Source source mark)
     :default (impl/at? source mark)))

(defn backtrack!
  #?(:clj {:inline (fn [source mark] `(.backtrack ~(tag source) ~mark))})
  [source mark]
  #?(:bb      (impl/backtrack! source mark)
     :clj     (.backtrack ^Source source mark)
     :default (impl/backtrack! source mark)))

;; ICharSource

(defn peek-str
  #?(:clj {:inline (fn [source n] `(.peekString ~(tag source) ~n))})
  [source n]
  #?(:bb      (impl/peek-str source n)
     :clj     (.peekString ^Source source n)
     :default (impl/peek-str source n)))

(defn read-str!
  #?(:clj {:inline (fn [source n] `(.readString ~(tag source) ~n))})
  [source n]
  #?(:bb      (impl/read-str! source n)
     :clj     (.readString ^Source source n)
     :default (impl/read-str! source n)))

(defn satisfies-char-pred?
  #?(:clj {:inline (fn [source pred]
                     `(.satisfies ~(tag source) ~(tag pred 'paco.detail.jvm.ICharPredicate)))})
  [source pred]
  #?(:bb      (impl/satisfies-char-pred? source pred)
     :clj     (.satisfies ^Source source ^ICharPredicate pred)
     :default (impl/satisfies-char-pred? source pred)))

(defn matches-str?
  #?(:clj {:inline (fn [source s] `(.matchesString ~(tag source) ~s))})
  [source s]
  #?(:bb      (impl/matches-str? source s)
     :clj     (.matchesString ^Source source s)
     :default (impl/matches-str? source s)))

(defn matches-str-ci?
  #?(:clj {:inline (fn [source s] `(.matchesStringIgnoreCase ~(tag source) ~s))})
  [source s]
  #?(:bb      (impl/matches-str-ci? source s)
     :clj     (.matchesStringIgnoreCase ^Source source s)
     :default (impl/matches-str-ci? source s)))

(defn re-match
  #?(:clj {:inline (fn [source re] `(.matchRegex ~(tag source) ~re))})
  [source re]
  #?(:bb      (impl/re-match source re)
     :clj     (.matchRegex ^Source source re)
     :default (impl/re-match source re)))

(defn re-groups [source re]
  (when-let [m (re-match source re)]
    #?(:clj  (clojure.core/re-groups m)
       :cljs (if (== (.-length m) 1)
               (aget m 0)
               (vec m)))))

(defn read-char-when!
  "Reads and returns the next character when it satisfies `pred`.  Returns
   `false` when it does not satsify `pred`, and `nil` at the end of the
   input stream."
  [source pred]
  #?(:bb      (impl/read-char-when! source pred)
     :clj     (let [ch (.readCharWhen ^Source source ^ICharPredicate pred)]
                (case ch -1 nil -2 false (char ch)))
     :default (impl/read-char-when! source pred)))

(defn skip-chars-while!
  #?(:clj {:inline (fn [source pred]
                     `(.skipCharsWhile ~(tag source) ~(tag pred 'paco.detail.jvm.ICharPredicate)))})
  [source pred]
  #?(:bb      (impl/skip-chars-while! source pred)
     :clj     (.skipCharsWhile ^Source source ^ICharPredicate pred)
     :default (impl/skip-chars-while! source pred)))

(defn read-from
  #?(:clj {:inline (fn [source mark] `(.readFrom ~(tag source) ~mark))})
  [source mark]
  #?(:bb      (impl/read-from source mark)
     :clj     (.readFrom ^Source source mark)
     :default (impl/read-from source mark)))

;; IUserStateSource

(defn modcount
  #?(:clj {:inline (fn [source] `(.modCount ~(tag source)))})
  [source]
  #?(:bb      (impl/modcount source)
     :clj     (.modCount ^Source source)
     :default (impl/modcount source)))

(defn backtrack-modified!
  #?(:clj {:inline (fn [source state] `(.backtrackModified ~(tag source) ~state))})
  [source state]
  #?(:bb      (impl/backtrack-modified! source state)
     :clj     (.backtrackModified ^Source source state)
     :default (impl/backtrack-modified! source state)))

(defn user-state
  #?(:clj {:inline (fn [source] `(.getUserState ~(tag source)))})
  [source]
  #?(:bb      (impl/user-state source)
     :clj     (.getUserState ^Source source)
     :default (impl/user-state source)))

(defn with-user-state!
  #?(:clj {:inline (fn [source state] `(doto ~(tag source) (.setUserState ~state)))})
  [source state]
  #?(:bb      (impl/with-user-state! source state)
     :clj     (doto ^Source source (.setUserState state))
     :default (impl/with-user-state! source state)))

;; ILineTrackingSource

#?(:clj (defn -long->position [^long p]
          (pos/position (bit-shift-right p 32) (bit-and p 0xffffffff))))

(defn position
  #?(:clj {:inline (fn
                     ([source]
                      `(-long->position (.position ~(tag source))))
                     ([source index]
                      `(-long->position (.position ~(tag source) ~index))))})
  ([source]
   #?(:bb      (impl/position source)
      :clj     (-long->position (.position ^Source source))
      :default (impl/position source)))
  ([source index]
   #?(:bb      (impl/position source index)
      :clj     (-long->position (.position ^Source source index))
      :default (impl/position source index))))

(defn untracked-skip!
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
  ([input]
   (of input nil))
  ([input opts]
   #?(:bb      (impl/of input opts)
      :clj     (Source/of ^String input (:user-state opts) (boolean (:line-tracking? opts true)))
      :default (impl/of input opts))))
