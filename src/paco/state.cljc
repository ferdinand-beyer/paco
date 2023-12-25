(ns paco.state
  (:refer-clojure :exclude [peek])
  (:require #?(:cljs [goog.string :as gstr])
            [paco.pos :as pos]))

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
  (skip [state] [state n]
    "Advances the state by up to `n` tokens."))

(defprotocol ICharState
  (peek-str [state n]
    "Peeks up to `n` characters and returns them as a string.")
  (matches-str? [state s]
    "Returns true if the next characters match `s`.")
  (matches-str-i? [state s]
    "Returns true if the next characters match `s`, ignoring case.")
  (untracked-skip [state] [state n]
    "Advances the state by up to `n` tokens without line tracking."))

(deftype CharEndState [^#?(:clj int, :cljs number) start
                       ^#?(:clj int, :cljs number) line
                       ^#?(:clj int, :cljs number) line-start
                       user]
  IState
  (index [_] start)
  (at-end? [_] true)
  (peek [_] nil)
  (peek [_ _] nil)
  (user-state [_] user)
  (with-user-state [this u]
    (if (identical? user u)
      this
      (CharEndState. start line line-start u)))
  (skip [this] this)
  (skip [this _] this)

  ICharState
  (peek-str [_ _] nil)
  (matches-str? [_ s] (empty? s))
  (matches-str-i? [_ s] (empty? s))
  (untracked-skip [this] this)
  (untracked-skip [this _] this)

  pos/IPosition
  (line-index [_] line)
  (column-index [_] (unchecked-subtract-int start line-start)))

;; TODO: Split this again, so that we can combine those?
;; - ISource, ICharSource -> EndCharSource, StringSource
;; - LineTracker (track [ch] [ch1 ch2])
;; - State, CharState
;; E.g. disable line tracking

;; TODO: Do we gain speed by using a mutating state?
;; For backtracking, we could seek or manually copy ("fork")

(deftype StringState [^String input
                      ^#?(:clj char, :cljs String) ch
                      ^#?(:clj int, :cljs number) start
                      ^#?(:clj int, :cljs number) end
                      ^#?(:clj int, :cljs number) line
                      ^#?(:clj int, :cljs number) line-start
                      user]
  IState
  (index [_] start)
  (at-end? [_] false)
  (peek [_] ch)
  (peek [_ offset]
    (let [index (unchecked-add-int start offset)]
      (when (< index end)
        (.charAt input index))))
  (user-state [_] user)
  (with-user-state [this u]
    (if (identical? user u)
      this
      (StringState. input ch start end line line-start u)))
  (skip [_]
    (let [next-start (unchecked-inc-int start)]
      (if (< next-start end)
        (let [next-ch (.charAt input next-start)]
          (if (or (= \newline ch)
                  (and (= \return ch) (not= \newline next-ch)))
            (StringState. input next-ch next-start end (unchecked-inc-int line) next-start user)
            (StringState. input next-ch next-start end line line-start user)))
        (if (or (= \newline ch) (= \return ch))
          (CharEndState. end (unchecked-inc-int line) end user)
          (CharEndState. end line line-start user)))))
  (skip [this n]
    (let [n (int n)]
      (if (pos? n)
        (let [skip-end (unchecked-add-int start n)
              stop (if (< skip-end end)
                     skip-end
                     (unchecked-dec-int end))]
          (loop [start start
                 ch ch
                 line line
                 line-start line-start]
            (if (< start stop)
              (let [next-start (unchecked-inc-int start)
                    next-ch (.charAt input next-start)]
                (if (or (= \newline ch)
                        (and (= \return ch) (not= \newline next-ch)))
                  (recur next-start next-ch (unchecked-inc-int line) next-start)
                  (recur next-start next-ch line line-start)))
              (if (= stop skip-end)
                (StringState. input ch start end line line-start user)
                (if (or (= \newline ch) (= \return ch))
                  (CharEndState. end (unchecked-inc-int line) end user)
                  (CharEndState. end line line-start user))))))
        this)))

  ICharState
  (peek-str [_ n]
    (.substring input start (min (unchecked-add-int start n) end)))
  (matches-str? [_ s]
    #?(:clj  (.regionMatches input start s 0 (.length ^String s))
       :cljs (let [e (unchecked-add-int start (.-length s))]
               (and (<= e end) (= (.substring input start e) s)))))
  (matches-str-i? [_ s]
    #?(:clj  (.regionMatches input true start s 0 (.length ^String s))
       :cljs (let [e (unchecked-add-int start (.-length s))]
               (and (<= e end) (gstr/caseInsensitiveEquals (.substring input start e) s)))))
  (untracked-skip [_]
    (let [start (unchecked-inc-int start)]
      (if (< start end)
        (StringState. input (.charAt input start) start end line line-start user)
        (CharEndState. end line line-start user))))
  (untracked-skip [this n]
    (if (pos? n)
      (let [start (unchecked-add-int start n)]
        (if (< start end)
          (StringState. input (.charAt input start) start end line line-start user)
          (CharEndState. end line line-start user)))
      this))

  pos/IPosition
  (line-index [_] line)
  (column-index [_] (unchecked-subtract-int start line-start)))

(defn of-string
  ([s]
   (of-string s nil))
  ([^String s user-state]
   (let [len #?(:clj (.length s), :cljs (.-length s))]
     (if (pos? len)
       (StringState. s (.charAt s 0) 0 len 0 0 user-state)
       (CharEndState. 0 0 0 user-state)))))

;; TODO: Support different input types and options (e.g. starting pos)?
(defn of [input _opts]
  (of-string input))

(defn pos [state]
  (pos/->Position (pos/line-index state) (pos/column-index state)))
