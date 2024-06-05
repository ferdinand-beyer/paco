(ns paco.detail.source.impl
  "Pure Clojure(Script) source implementation."
  (:refer-clojure :exclude [peek])
  (:require #?@(:cljs [[goog.array :as garr]
                       [goog.string :as gstr]])
            [paco.detail.position :as pos])
  #?(:clj (:import [java.util ArrayList Collections]
                   [java.util.regex Pattern])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IReleasable
  (-release! [this]))

(defn release! [x]
  (when (satisfies? IReleasable x)
    (-release! x)))

(defprotocol ISource
  (index [source])
  (end? [source])
  (peek [source])
  (skip! [source] [source n])
  (mark [source])
  (at? [source mark])
  (backtrack! [source mark]))

(defprotocol ICharSource
  (peek-str [source n])
  (read-str! [source n])
  (satisfies-char-pred? [source pred])
  (matches-str? [source s])
  (matches-str-ci? [source s])
  (re-match [source re])
  (read-char-when! [source pred])
  (skip-chars-while! [source pred])
  (read-from [source mark]))

(deftype StringSource #?(:clj  [^String input
                                ^int end
                                ^:unsynchronized-mutable ^int index*]
                         :cljs [^string input
                                ^number end
                                ^:mutable ^number index*])
  ISource
  (index [_] index*)
  (end? [_] (>= index* end))
  (peek [_]
    (when (< index* end)
      (.charAt input index*)))
  (skip! [_]
    (if (< index* end)
      (do (set! index* (unchecked-inc-int index*)) 1)
      0))
  (skip! [_ n]
    (let [index (Math/min (unchecked-add-int index* (int n)) end)
          k     (unchecked-subtract-int index index*)]
      (set! index* index)
      k))
  (mark [_] index*)
  (at? [_ index] (= index index*))
  (backtrack! [this index]
    (set! index* (int index))
    this)

  ICharSource
  (peek-str [_ n]
    (when (< index* end)
      (.substring input index* (Math/min (unchecked-add-int index* (int n)) end))))
  (read-str! [_ n]
    (when (< index* end)
      (let [s (.substring input index* (Math/min (unchecked-add-int index* (int n)) end))]
        (set! index* (unchecked-add-int index* (count s)))
        s)))
  (satisfies-char-pred? [_ pred]
    (and (< index* end) (pred (.charAt input index*))))
  (matches-str? [_ s]
    #?(:clj  (.regionMatches input index* s 0 (.length ^String s))
       :cljs (let [e (unchecked-add-int index* (.-length s))]
               (and (<= e end) (= (.substring input index* e) s)))))
  (matches-str-ci? [_ s]
    #?(:clj  (.regionMatches input true index* s 0 (.length ^String s))
       :cljs (let [e (unchecked-add-int index* (.-length s))]
               (and (<= e end)
                    (gstr/caseInsensitiveEquals (.substring input index* e) s)))))
  (re-match [_ re]
    (when (< index* end)
      #?(:clj (let [m (.. ^Pattern re (matcher input)
                          (region index* end)
                          ;; Don't match ^, for compatibility with JavaScript.
                          (useAnchoringBounds false))]
                (when (.lookingAt m)
                  m))
         :cljs (let [re* (if (and (.-sticky re) (not (.-global re)))
                           re
                           (js/RegExp. re (.. re -flags (replace #"[gy]" "") (concat "y"))))]
                 (set! (.-lastIndex re*) index*)
                 (.exec re* input)))))
  (read-char-when! [_ pred]
    (when (< index* end)
      (let [ch (.charAt input index*)]
        (if (pred ch)
          (do
            (set! index* (unchecked-inc-int index*))
            ch)
          false))))
  (skip-chars-while! [_ pred]
    (loop [i index*]
      (if (and (< i end) (pred (.charAt input i)))
        (recur (unchecked-inc-int i))
        (let [k (unchecked-subtract-int i index*)]
          (set! index* k)
          k))))
  (read-from [_ mark] (subs input mark index*)))

(defn- string-source [^String s]
  (StringSource. s #?(:clj (.length s) :cljs (.-length s)) 0))

(defprotocol IUserStateSource
  (modcount [source])
  (backtrack-modified! [source mark])
  (user-state [source])
  (with-user-state! [source user-state]))

(defprotocol ILineTrackingSource
  (position [source] [source index])
  (untracked-skip! [source] [source n]))

(defprotocol ILineTracker
  (-track! [tracker index ch] [tracker index ch next-ch])
  (-track-skip! [tracker source] [tracker source n])
  (-track-skip-while! [tracker source pred])
  (-position [tracker index]))

(deftype SourceMark #?(:clj  [mark ^long modcount user-state]
                       :cljs [mark ^number modcount user-state]))

(deftype Source #?(:clj  [source
                          line-tracker
                          ^:unsynchronized-mutable ^long modcount*
                          ^:unsynchronized-mutable user-state*]
                   :cljs [source
                          line-tracker
                          ^:mutable ^number modcount*
                          ^:mutable user-state*])
  ISource
  (index [_] (index source))
  (end? [_] (end? source))
  (peek [_] (peek source))
  (skip! [_]
    (let [k (if line-tracker
              (-track-skip! line-tracker source)
              (skip! source))]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (skip! [_ n]
    (let [k (if line-tracker
              (-track-skip! line-tracker source n)
              (skip! source n))]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (mark [_]
    (SourceMark. (mark source) modcount* user-state*))
  (at? [_ mark]
    (= modcount* (.-modcount ^SourceMark mark)))
  (backtrack! [this mark]
    (backtrack! source (.-mark ^SourceMark mark))
    (set! modcount* (.-modcount ^SourceMark mark))
    (set! user-state* (.-user-state ^SourceMark mark))
    this)

  ICharSource
  (peek-str [_ n] (peek-str source n))
  (read-str! [_ n]
    (let [s (if line-tracker
              (when-let [s (peek-str source n)]
                (-track-skip! line-tracker source n)
                s)
              (read-str! source n))]
      (when-not (zero? (count s))
        (set! modcount* (unchecked-inc modcount*)))
      s))
  (satisfies-char-pred? [_ pred] (satisfies-char-pred? source pred))
  (matches-str? [_ s] (matches-str? source s))
  (matches-str-ci? [_ s] (matches-str-ci? source s))
  (re-match [_ re] (re-match source re))
  (read-char-when! [_ pred]
    (let [ch (read-char-when! source pred)]
      (when ch
        (set! modcount* (unchecked-inc modcount*))
        (when line-tracker
          (-track! line-tracker (unchecked-dec (index source)) ch (peek source))))
      ch))
  (skip-chars-while! [_ pred]
    (let [k (if line-tracker
              (-track-skip-while! line-tracker source pred)
              (skip-chars-while! source pred))]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (read-from [_ mark] (read-from source (.-mark ^SourceMark mark)))

  IUserStateSource
  (modcount [_] modcount*)
  (backtrack-modified! [this mark]
    (backtrack! source (.-mark ^SourceMark mark))
    (set! modcount* (unchecked-inc modcount*))
    (set! user-state* (.-user-state ^SourceMark mark))
    this)
  (user-state [_] user-state*)
  (with-user-state! [this user-state]
    (when-not (identical? user-state user-state*)
      (set! modcount* (unchecked-inc modcount*))
      (set! user-state* user-state))
    this)

  ILineTrackingSource
  (position [_]
    (if line-tracker
      (-position line-tracker (index source))
      (pos/position 0 (index source))))
  (position [_ index]
    (if line-tracker
      (-position line-tracker index)
      (pos/position 0 index)))
  (untracked-skip! [_]
    (let [k (skip! source)]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (untracked-skip! [_ n]
    (let [k (skip! source n)]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k)))

(defn- make-source [source line-tracker user-state]
  (Source. source line-tracker 0 user-state))

(deftype LineTracker #?(:clj [^ArrayList starts
                              ^:unsynchronized-mutable ^int max-index*]
                        :cljs [^array starts
                               ^:mutable ^number max-index*])
  ILineTracker
  (-track! [this index ch] (-track! this index ch nil))
  (-track! [this index ch next-ch]
    (let [index (int index)]
      (when (> index max-index*)
        (set! max-index* index)
        (when (or (= ch \newline)
                  ;; For \r\n, could track (index + 2) as a line start,
                  ;; and inc max-index once more.
                  (and (= ch \return) (not= next-ch \newline)))
          #?(:clj  (.add starts (unchecked-inc-int index))
             :cljs (.push starts (unchecked-inc-int index))))))
    this)
  (-track-skip! [this source]
    (let [index (index source)
          ch    (peek source)
          k     (skip! source)]
      (-track! this index ch (peek source))
      k))
  (-track-skip! [this source n]
    (let [n (int n)]
      (loop [k (int 0)
             ch (peek source)]
        (if (and ch (< k n))
          (let [index (index source)
                k (unchecked-add-int k (skip! source))
                next-ch (peek source)]
            (-track! this index ch next-ch)
            (recur k next-ch))
          k))))
  (-track-skip-while! [this source pred]
    (loop [k (int 0)
           ch (peek source)]
      (if (and ch (pred ch))
        (let [index (index source)
              k (unchecked-add-int k (skip! source))
              next-ch (peek source)]
          (-track! this index ch next-ch)
          (recur k next-ch))
        k)))

  (-position [_ index]
    (let [index (int index)
          i #?(:clj  (Collections/binarySearch starts index)
               :cljs (garr/binarySearch starts index))]
      (if (< i -1)
        (let [line (-> i unchecked-negate-int unchecked-dec-int)
              k    (unchecked-dec-int line)]
          (pos/position line (unchecked-subtract-int index #?(:clj  (.get starts k)
                                                              :cljs (aget starts k)))))
        (if (>= i 0)
          (pos/position (unchecked-inc-int i) 0)
          (pos/position 0 index))))))

(defn- line-tracker []
  (LineTracker. #?(:clj (ArrayList.) :cljs #js []) -1))

(defn of
  ([input] (of input nil))
  ([input {:keys [user-state line-tracking?]
           :or {line-tracking? true}}]
   (make-source (string-source input)
                (when line-tracking?
                  (line-tracker))
                user-state)))
