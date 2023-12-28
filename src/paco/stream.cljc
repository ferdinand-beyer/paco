(ns paco.stream
  #?(:clj (:import [paco.impl
                    CharPredicate
                    ICharScanner
                    IScanner
                    LineTracker
                    StringScanner])))

#?(:clj (set! *warn-on-reflection* true))

(deftype SavePoint [^long mod-count ^long index user-state])

(defprotocol IState
  (mod-count [state])
  (save-point [state])
  (backtrack! [state save-point]))

(defprotocol IInternalState
  (internal-mod-count [state])
  (internal-modified! [state])
  (internal-save-point [state scanner])
  (internal-backtrack! [state scanner save-point]))

(deftype InternalState [^#?(:clj :unsynchronized-mutable, :cljs :mutable) ^long mod-count*
                        ^#?(:clj :unsynchronized-mutable, :cljs :mutable) user-state*]
  IInternalState
  (internal-mod-count [_] mod-count*)
  (internal-modified! [_] (set! mod-count* (unchecked-inc mod-count*)))
  (internal-save-point [_ scanner]
    (SavePoint. mod-count* (.index ^IScanner scanner) user-state*))
  (internal-backtrack! [_ scanner save-point]
    (.seek ^IScanner scanner (.-index ^SavePoint save-point))
    (set! mod-count* (.-mod-count ^SavePoint save-point))
    (set! user-state* (.-user-state ^SavePoint save-point))))

(defn- internal-state []
  (InternalState. 0 nil))

(defmacro ^:private modified-when-skipped [internal-state skip-call]
  `(let [n# ~skip-call]
     (when (pos? n#)
       (.internal-modified! ~internal-state))
     n#))

#?(:clj
   (deftype GenericState [^InternalState internal ^IScanner scanner]
     IState
     (mod-count [_] (.internal-mod-count internal))
     (save-point [_] (.internal-save-point internal scanner))
     (backtrack! [_ save-point] (.internal-backtrack! internal scanner save-point))

     IScanner
     (index [_] (.index scanner))
     (atEnd [_] (.atEnd scanner))
     (peekToken [_] (.peekToken scanner))
     (matchesToken [_ token] (.matchesToken scanner token))
     (skip [_]
       (modified-when-skipped internal (.skip scanner)))
     (skip [_ n]
       (modified-when-skipped internal (.skip scanner n)))
     (skipWhile [_ pred]
       (modified-when-skipped internal (.skipWhile scanner pred)))
     (seek [_ index]
       (.internal-modified! internal)
       (.seek scanner index))))

#?(:clj
   (deftype CharState [^InternalState internal
                       ^ICharScanner scanner
                       ^LineTracker line-tracker]
     IState
     (mod-count [_] (.internal-mod-count internal))
     (save-point [_] (.internal-save-point internal scanner))
     (backtrack! [_ save-point] (.internal-backtrack! internal scanner save-point))

     IScanner
     (index [_] (.index scanner))
     (atEnd [_] (.atEnd scanner))
     (peekToken [_] (.peekToken scanner))
     (matchesToken [_ token] (.matchesToken scanner token))
     (skip [_]
       (modified-when-skipped internal (if line-tracker
                                         (.skip line-tracker scanner)
                                         (.skip scanner))))
     (skip [_ n]
       (modified-when-skipped internal (if line-tracker
                                         (.skip line-tracker scanner n)
                                         (.skip scanner n))))
     (skipWhile [this pred]
       (.skipCharsWhile this (CharPredicate/of pred)))
     (seek [_ index]
       (.internal-modified! internal)
       (.seek scanner index))

     ICharScanner
     (peekChar [_] (.peekChar scanner))
     (peekString [_ n] (.peekString scanner n))
     (matches [_ pred] (.matches scanner pred))
     (matchesChar [_ ch] (.matchesChar scanner ch))
     (matchesString [_ s] (.matchesString scanner s))
     (matchesStringCI [_ s] (.matchesStringCI scanner s))
     (match [_ p] (.match scanner p))
     (skipCharsWhile [_ pred]
       (modified-when-skipped internal
                              (if line-tracker
                                (.skipCharsWhile line-tracker scanner pred)
                                (.skipCharsWhile scanner pred))))))

(defn char-state ^CharState [input]
  (CharState. (internal-state) (StringScanner. input) (LineTracker.)))

(defn read-rest-of-line [^ICharScanner scanner]
  (when-let [m (.match scanner #"^[^\r\n]*(?:\n|\r\n?|$)")]
    (let [s (.group m)]
      (.skip scanner (.length s))
      s)))

(comment
  (require '[criterium.core :as criterium])
  (def input (slurp "dev/experiments/citm_catalog.json"))

  ;; find the first 'x' (120) on 8344, Ln 290, Col 28
  (.index scanner)
  (mod-count scanner)
  (.. scanner -line-tracker (position (.index scanner)))

  (let [scanner (char-state input)]
    (.skipWhile scanner (fn [^long ch] (not= 120 ch))))

  (do
    (def scanner (char-state input))
    (let [s ^CharState scanner]
      (criterium/quick-bench
       (.skipWhile s (fn [^long ch] (not= 120 ch))))))
  ;; Execution time mean : 642,200156 ns
  ;; Execution time mean : 7,542247 ns

  (do
    (def scanner (char-state input))
    (let [s ^CharState scanner]
      (criterium/quick-bench
       (.skipCharsWhile s (CharPredicate/notEquals \x)))))
  ;; Execution time mean : 583,029694 ns
  ;; Execution time mean : 4,303737 ns

  (let [scanner (char-state input)]
    (criterium/quick-bench
     (.skipWhile scanner (constantly true))))
  ;; Execution time mean : 637,123359 ns
  ;; Execution time mean : 6,246791 ns

  (do
    (def scanner (char-state input))
    (let [s ^CharState scanner]
      (criterium/quick-bench
       (.skipWhile s (constantly true)))))
  ;; Execution time mean : 585,414406 ns
  ;; Execution time mean : 6,163118 ns

  (criterium/quick-bench
   (reduce (fn [^long n _] (unchecked-inc n)) 0 input))
  ;; Execution time mean : 3,539938 ms

  (criterium/quick-bench
   (loop [i (.length ^String input)
          n (int 0)]
     (if (pos? i)
       (recur (unchecked-dec-int i) (unchecked-inc-int n))
       n)))
  ;; Execution time mean : 109,575511 µs

  (def ^ICharScanner scanner (char-state input))
  (read-rest-of-line scanner)

  ;;
  )
