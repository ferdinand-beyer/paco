(ns comparse.state
  #?(:cljs (:refer-clojure :exclude [-peek])))

(defprotocol IStream
  (char-index [stream])
  (peek-char [stream]
    "Return the next character in the input stream, or `nil`.")
  (peek-str [stream n])
  (skip* [stream n]
    "Advances the state by n characters."))

(defn at-end? [stream]
  (nil? (peek-char stream)))

(defn skip-char* [stream]
  (skip* stream 1))

(deftype EndStream [index]
  IStream
  (char-index [_] index)
  (peek-char [_] nil)
  (peek-str [_ _] nil)
  (skip* [this _] this))

(deftype StringStream [^String string ^int index]
  IStream
  (char-index [_] index)
  (peek-char [_] (.charAt string index))
  (peek-str [_ n]
    (subs string index
          (min (unchecked-add index n)
               (.length string))))
  (skip* [_ n]
    (let [new-index (unchecked-add index n)]
      (if (< new-index (.length string))
        (StringStream. string new-index)
        (EndStream. (.length string))))))

(defn string-stream [^String string]
  (if (.isEmpty string)
    (EndStream. 0)
    (StringStream. string 0)))

(deftype Position [char line column])

(deftype State [stream ^int line ^int line-begin user-state]
  IStream
  (char-index [_] (char-index stream))
  (peek-char [_] (peek-char stream))
  (peek-str [_ n] (peek-str stream n))
  ;; Does not track line numbers.
  (skip* [_ n] (State. (skip* stream n) line line-begin user-state)))

(defn of-string
  ([string]
   (of-string string nil))
  ([string user-state]
   (State. (string-stream string) 0 0 user-state)))

(defn line-index [^State state]
  (.-line state))

(defn column-index [^State state]
  (unchecked-subtract (char-index state) (.-line-begin state)))

(defn position [state]
  (Position. (char-index state)
             (line-index state)
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
          (char-index stream)
          (.-user-state state)))

(defn skip-char
  "Skips to the next character, tracking line numbers."
  [^State state]
  (let [stream (.-stream state)]
    (if-let [c (peek-char stream)]
      (let [new-stream (skip* stream 1)]
        (case c
          \return (if (= \newline (peek-char new-stream))
                    (same-line state new-stream)
                    (next-line state new-stream))
          \newline (next-line state new-stream)
          (same-line state new-stream)))
      state)))

;; TODO: Need StateTag?
;; Modification counter
(defn changed? [state1 state2]
  (not= (char-index state1)
        (char-index state2)))

(defn matches? [state ^String s]
  (= s (peek-str state (.length s))))
