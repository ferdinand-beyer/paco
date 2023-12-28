(ns paco.stream
  #?(:clj (:import [paco.impl
                    CharPredicate
                    ICharScanner
                    IScanner
                    LineTracker
                    StringScanner])))

#?(:clj (set! *warn-on-reflection* true))

;; Conditional macros:

;; bb: #?(:bb)
;; cljs: (:ns &env)

(defprotocol IState
  (user-state [state])
  (set-user-state! [state user-state]))

#?(:clj
   (deftype State [^IScanner scanner user-state]
     IScanner
     (index [_] (.index scanner))
     (atEnd [_] (.atEnd scanner))
     (peekToken [_] (.peekToken scanner))
     (matchesToken [_ token] (.matchesToken scanner token))
     (skip [_] (.skip scanner))
     (skip [_ n] (.skip scanner n))
     (skipWhile [_ pred] (.skipWhile scanner pred))
     (seek [_ index] (.seek scanner index))))

#?(:clj
   (deftype CharState [^ICharScanner scanner
                       ^LineTracker line-tracker
                       user-state]
     IScanner
     (index [_] (.index scanner))
     (atEnd [_] (.atEnd scanner))
     (peekToken [_] (.peekToken scanner))
     (matchesToken [_ token] (.matchesToken scanner token))
     (skip [_]
       (if line-tracker
         (.skip line-tracker scanner)
         (.skip scanner)))
     (skip [_ n]
       (if line-tracker
         (.skip line-tracker scanner n)
         (.skip scanner n)))
     (skipWhile [this pred]
       (.skipCharsWhile this (CharPredicate/of pred)))
     (seek [_ index] (.seek scanner index))

     ICharScanner
     (peekChar [_] (.peekChar scanner))
     (peekString [_ n] (.peekString scanner n))
     (matches [_ pred] (.matches scanner pred))
     (matchesChar [_ ch] (.matchesChar scanner ch))
     (matchesString [_ s] (.matchesString scanner s))
     (matchesStringCI [_ s] (.matchesStringCI scanner s))
     (match [_ p] (.match scanner p))
     (skipCharsWhile [_ pred]
       (if line-tracker
         (.skipCharsWhile line-tracker scanner pred)
         (.skipCharsWhile scanner pred)))))

(defn read-rest-of-line [^ICharScanner scanner]
  (when-let [m (.match scanner #"^[^\r\n]*(?:\n|\r\n?|$)")]
    (let [s (.group m)]
      (.skip scanner (.length s))
      s)))

(comment
  (require '[criterium.core :as criterium])
  (def input (slurp "src/paco/stream.cljc"))

  ;; The X marks the spot.

  (let [scanner (CharState. (StringScanner. input) (LineTracker.) nil)]
    (.skipWhile scanner (fn [^long ch] (not= 88 ch))))

  (let [scanner (CharState. (StringScanner. input) (LineTracker.) nil)]
    (criterium/quick-bench
     (.skipWhile scanner (fn [^long ch] (not= 88 ch)))))
  ;; Execution time mean : 7,635343 ns

  (let [scanner (CharState. (StringScanner. input) (LineTracker.) nil)]
    (criterium/quick-bench
     (.skipCharsWhile scanner (CharPredicate/equals \X))))
  ;; Execution time mean : 3,482621 ns

  (let [scanner (CharState. (StringScanner. input) (LineTracker.) nil)]
    (criterium/quick-bench
     (.skipWhile scanner (complement #{\X}))))
  ;; Execution time mean : 20,988696 ns

  (let [scanner (CharState. (StringScanner. input) (LineTracker.) nil)]
    (criterium/quick-bench
     (.skipWhile scanner (fn [ch] (not= \X ch)))))
  ;; Execution time mean : 12,439039 ns

  (let [scanner (CharState. (StringScanner. input) (LineTracker.) nil)]
    (criterium/quick-bench
     (.skipWhile scanner (constantly true))))
  ;; Execution time mean : 8,943564 ns

  (def ^ICharScanner scanner (CharState. (StringScanner. input) (LineTracker.) nil))
  (read-rest-of-line scanner)

  ;;
  )
