(ns paco.state
  (:refer-clojure :exclude [peek])
  (:require #?(:cljs [goog.string :as gstr])
            [paco.pos :as pos])
  #?(:clj (:import [clojure.lang IPersistentMap])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IState
  (index [state]
    "Returns the index of the next token.")
  (at-end? [state]
    "Returns true if this is the end of the input.")
  (peek [state] [state offset]
    "Return the next token in the stream, or `nil` when at the end.")
  (user-state [state]
    "Returns the user-state.")
  (with-user-state [state u]
    "Returns a new state with `u` as the user state.")
  (skip [state n]
    "Advances the state by up to `n` tokens."))

(defprotocol ICharState
  (peek-str [state n]
    "Peeks up to `n` characters and returns them as a string.")
  (matches-str? [state s]
    "Returns true if the next characters match `s`.")
  (matches-str-i? [state s]
    "Returns true if the next characters match `s`, ignoring case.")
  (untracked-skip [state n]
    "Advances the state by up to `n` tokens without line tracking."))

(deftype StringState [^String input
                      ^#?(:clj int, :cljs number) start
                      ^#?(:clj int, :cljs number) end
                      ^#?(:clj int, :cljs number) line
                      ^#?(:clj int, :cljs number) line-start
                      ^IPersistentMap user]
  IState
  (index [_] start)
  (at-end? [_] (= start end))
  (peek [_] (when (< start end) (.charAt input start)))
  (peek [_ offset]
    (let [index (unchecked-add start offset)]
      (when (< index end)
        (.charAt input (unchecked-add start offset)))))
  (user-state [_] user)
  (with-user-state [this u]
    (if (identical? user u)
      this
      (StringState. input start end line line-start u)))
  (skip [this n]
    (if (pos? n)
      (loop [i (min (int n) (unchecked-subtract-int end start))
             start start
             line line
             line-start line-start]
        (if (pos? i)
          (let [next-start (inc start)]
            (case (.charAt input start)
              \return (if (and (< next-start end)
                               (= \newline (.charAt input next-start)))
                        (recur (dec i) next-start line line-start)
                        (recur (dec i) next-start (inc line) next-start))
              \newline (recur (dec i) next-start (inc line) next-start)
              (recur (dec i) next-start line line-start)))
          (StringState. input start end line line-start user)))
      this))

  ICharState
  (peek-str [_ n]
    (when (< start end)
      (.substring input start (min (unchecked-add start n) end))))
  (matches-str? [_ s]
    #?(:clj  (.regionMatches input start s 0 (.length ^String s))
       :cljs (let [e (unchecked-add start (.-length s))]
               (and (<= e end) (= (.substring input start e) s)))))
  (matches-str-i? [_ s]
    #?(:clj  (.regionMatches input true start s 0 (.length ^String s))
       :cljs (let [e (unchecked-add start (.-length s))]
               (and (<= e end) (gstr/caseInsensitiveEquals (.substring input start e) s)))))
  (untracked-skip [this n]
    (if (pos? n)
      (StringState. input (min (unchecked-add start n) end) end line line-start user)
      this))

  pos/IPosition
  (line-index [_] line)
  (column-index [_] (unchecked-subtract start line-start)))

(defn of-string
  ([s]
   (of-string s nil))
  ([s user-state]
   (StringState. s 0 #?(:clj (.length ^String s), :cljs (.-length ^js s)) 0 0 user-state)))

;; TODO: Support different input types and options (e.g. starting pos)?
(defn of [input _opts]
  (of-string input))

(defn pos [state]
  (pos/->Position (pos/line-index state) (pos/column-index state)))
