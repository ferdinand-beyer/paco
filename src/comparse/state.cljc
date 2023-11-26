(ns comparse.state
  (:refer-clojure :exclude [peek]))

#?(:clj (set! *warn-on-reflection* true))

;;---------------------------------------------------------
;; IStream

(defprotocol IStream
  (index [stream])
  (peek [stream]
    "Return the next token in the stream, or `nil` when at the end.")
  (skip [stream n]
    "Advances the stream by `n` tokens, without position tracking."))

(defn at-end? [stream]
  (nil? (peek stream)))

;;---------------------------------------------------------
;; ICharStream

(defprotocol ICharStream
  (peek-str [stream n]
    "Peeks up to `n` characters and returns them as a string.")
  (matches-str? [stream s]
    "Returns true if the next characters match `s`.")
  (matches-str-ic? [stream s]
    "Returns true if the next characters match `s`, ignoring case."))

;;---------------------------------------------------------
;; EndStream

(deftype EndStream [index]
  IStream
  (index [_] index)
  (peek [_] nil)
  (skip [this _] this)

  ICharStream
  (peek-str [_ _] nil)
  (matches-str? [_ s] (empty? s))
  (matches-str-ic? [_ s] (empty? s)))

;;---------------------------------------------------------
;; StringStream

(deftype StringStream [^String input ^int index]
  IStream
  (index [_] index)
  (peek [_] (.charAt input index))
  (skip [_ n]
    (let [new-index (unchecked-add index n)]
      (if (< new-index (.length input))
        (StringStream. input new-index)
        (EndStream. (.length input)))))

  ICharStream
  (peek-str [_ n]
    (subs input index
          (min (unchecked-add index n)
               (.length input))))
  (matches-str? [_ s]
    (.regionMatches input index s 0 (.length ^String s)))
  (matches-str-ic? [_ s]
    (.regionMatches input true index s 0 (.length ^String s))))

(defn string-stream [^String input]
  (if (.isEmpty input)
    (EndStream. 0)
    (StringStream. input 0)))

;;---------------------------------------------------------
;; Position

(defprotocol IPosition
  (char-index [pos])
  (line-index [pos])
  (column-index [pos]))

(deftype Position [^int line ^int column]
  Object
  (toString [_] (str "line " (unchecked-inc line)
                     ", column " (unchecked-inc column)))

  IPosition
  (line-index [_] line)
  (column-index [_] column))

;;---------------------------------------------------------
;; State

;; ? Maybe support stream transform functions
;; - normalize newlines to \n

(deftype State [stream ^int line ^int line-begin user-state]
  IStream
  (index [_] (index stream))
  (peek [_] (peek stream))
  (skip [_ n] (State. (skip stream n) line line-begin user-state))

  ICharStream
  (peek-str [_ n] (peek-str stream n))
  (matches-str? [_ s] (matches-str? stream s))
  (matches-str-ic? [_ s] (matches-str-ic? stream s))

  IPosition
  (line-index [_] line)
  (column-index [_] (unchecked-subtract (index stream) line-begin)))

(defn of-string
  ([s]
   (of-string s nil))
  ([s user-state]
   (State. (string-stream s) 0 0 user-state))
  ([s user-state pos]
   (State. (string-stream s)
           (line-index pos)
           (- (column-index pos))
           user-state)))

(defn position [state]
  (Position. (line-index state)
             (column-index state)))

(defn user-state [^State state]
  (.-user-state state))

(defn- same-line [^State state stream]
  (State. stream
          (.-line state)
          (.-line-begin state)
          (.-user-state state)))

(defn- next-line [^State state stream]
  (State. stream
          (unchecked-inc (.-line state))
          (index stream)
          (.-user-state state)))

(defn skip-char
  "Skips to the next character, tracking line numbers."
  [^State state]
  (let [stream (.-stream state)]
    (if-let [c (peek stream)]
      (let [new-stream (skip stream 1)]
        (case c
          \return (if (= \newline (peek new-stream))
                    (same-line state new-stream)
                    (next-line state new-stream))
          \newline (next-line state new-stream)
          (same-line state new-stream)))
      state)))
