(ns paco.detail.scanner
  (:refer-clojure :exclude [peek re-groups])
  (:require #?@(:cljs [[goog.array :as garr]
                       [goog.string :as gstr]])
            [paco.detail.position :as pos])
  #?(:clj (:import [java.util ArrayList Collections]
                   [java.util.regex Pattern])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IScanner
  (index [scanner])
  (end? [scanner])
  (peek [scanner])
  (matches? [scanner pred])
  (skip! [scanner] [scanner n])
  (state [scanner])
  (in-state? [scanner state])
  (backtrack! [scanner state]))

(defprotocol ICharScanner
  (peek-str [scanner n])
  (matches-str? [scanner s])
  (matches-str-ci? [scanner s])
  (re-match [scanner re]))

(defn re-groups [scanner re]
  (when-let [m (re-match scanner re)]
    #?(:clj  (clojure.core/re-groups m)
       :cljs (if (== (.-length m) 1)
               (aget m 0)
               (vec m)))))

(deftype StringScanner #?(:clj  [^String input
                                 ^int end
                                 ^:unsynchronized-mutable ^int index*]
                          :cljs [^string input
                                 ^number end
                                 ^:mutable ^number index*])
  IScanner
  (index [_] index*)
  (end? [_] (>= index* end))
  (peek [_]
    (when (< index* end)
      (.charAt input index*)))
  (matches? [_ pred]
    (when (< index* end)
      (pred (.charAt input index*))))
  (skip! [_]
    (if (< index* end)
      (do (set! index* (unchecked-inc-int index*)) 1)
      0))
  (skip! [_ n]
    (let [index (Math/min (unchecked-add-int index* (int n)) end)]
      (set! index* index)
      (unchecked-subtract-int index index*)))
  (state [_] index*)
  (in-state? [_ index] (= index index*))
  (backtrack! [this index]
    (set! index* (int index))
    this)

  ICharScanner
  (peek-str [_ n]
    (when (< index* end)
      (.substring input index* (Math/min (unchecked-add-int index* (int n)) end))))
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
                 (.exec re* input))))))

(defn- string-scanner [^String s]
  (StringScanner. s #?(:clj (.length s) :cljs (.-length s)) 0))

(defprotocol IModCountScanner
  (modcount [scanner])
  (backtrack-modified! [scanner state]))

(defprotocol IUserStateScanner
  (user-state [scanner])
  (with-user-state! [scanner user-state]))

(defprotocol ILineTrackingScanner
  (position [scanner])
  (untracked-skip! [scanner] [scanner n]))

(defprotocol ILineTracker
  (-track! [tracker index ch] [tracker index ch next-ch])
  (-track-skip! [tracker scanner] [tracker scanner n])
  (-position [tracker index]))

(deftype ScannerState #?(:clj  [^long modcount ^int index user-state]
                         :cljs [^number modcount ^number index user-state]))

(deftype Scanner #?(:clj  [scanner
                           line-tracker
                           ^:unsynchronized-mutable ^long modcount*
                           ^:unsynchronized-mutable user-state*]
                    :cljs [scanner
                           line-tracker
                           ^:mutable ^number modcount*
                           ^:mutable user-state*])
  IScanner
  (index [_] (index scanner))
  (end? [_] (end? scanner))
  (peek [_] (peek scanner))
  (matches? [_ pred] (matches? scanner pred))
  (skip! [_]
    (let [k (if line-tracker
              (-track-skip! line-tracker scanner)
              (skip! scanner))]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (skip! [_ n]
    (let [k (if line-tracker
              (-track-skip! line-tracker scanner n)
              (skip! scanner n))]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (state [_]
    (ScannerState. modcount* (index scanner) user-state*))
  (in-state? [_ state]
    (= modcount* (.modcount ^ScannerState state)))
  (backtrack! [this state]
    (backtrack! scanner (.index ^ScannerState state))
    (set! modcount* (.modcount ^ScannerState state))
    (set! user-state* (.user-state ^ScannerState state))
    this)

  ICharScanner
  (peek-str [_ n] (peek-str scanner n))
  (matches-str? [_ s] (matches-str? scanner s))
  (matches-str-ci? [_ s] (matches-str-ci? scanner s))
  (re-match [_ re] (re-match scanner re))

  IModCountScanner
  (modcount [_] modcount*)
  (backtrack-modified! [this state]
    (backtrack! scanner (.index ^ScannerState state))
    (set! modcount* (unchecked-inc modcount*))
    (set! user-state* (.user-state ^ScannerState state))
    this)

  IUserStateScanner
  (user-state [_] user-state*)
  (with-user-state! [this user-state]
    (when-not (identical? user-state user-state*)
      (set! modcount* (unchecked-inc modcount*))
      (set! user-state* user-state))
    this)

  ILineTrackingScanner
  (position [_]
    (when line-tracker
      (-position line-tracker (index scanner))))
  (untracked-skip! [_]
    (let [k (skip! scanner)]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k))
  (untracked-skip! [_ n]
    (let [k (skip! scanner n)]
      (when-not (zero? k)
        (set! modcount* (unchecked-inc modcount*)))
      k)))

(defn- make-scanner [scanner line-tracker user-state]
  (Scanner. scanner line-tracker 0 user-state))

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
  (-track-skip! [this scanner]
    (let [index (index scanner)
          ch    (peek scanner)
          k     (skip! scanner)]
      (-track! this index ch (peek scanner))
      k))
  (-track-skip! [this scanner n]
    (let [n (int n)]
      (loop [k (int 0)
             ch (peek scanner)]
        (if (and ch (< k n))
          (let [index (index scanner)
                k (unchecked-add-int k (skip! scanner))
                next-ch (peek scanner)]
            (-track! this index ch next-ch)
            (recur k next-ch))
          k))))
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
   (make-scanner (string-scanner input)
                 (when line-tracking?
                   (line-tracker))
                 user-state)))

(comment
  (def scanner (of "foo\nbar\n"))
  (skip! scanner 5)
  (peek-str scanner 7)
  (position scanner)

  ;;
  )
