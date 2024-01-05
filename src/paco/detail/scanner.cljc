(ns paco.detail.scanner
  (:refer-clojure :exclude [peek re-groups])
  (:require #?(:bb   [paco.detail.scanner.default :as default]
               :cljs [paco.detail.scanner.default :as default])
            #?(:clj [paco.detail.position :as pos]))
  #?@(:bb  []
      :clj [(:import [paco.detail.jvm CharPredicate PacoScanner])]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (defn- tag
          ([x] (tag x 'paco.detail.jvm.PacoScanner))
          ([x tag]
           (vary-meta x assoc :tag tag))))

;; IScanner

(defn index
  #?(:clj {:inline (fn [scanner] `(.index ~(tag scanner)))})
  [scanner]
  #?(:bb      (default/index scanner)
     :clj     (.index ^PacoScanner scanner)
     :default (default/index scanner)))

(defn end?
  #?(:clj {:inline (fn [scanner] `(.isEnd ~(tag scanner)))})
  [scanner]
  #?(:bb      (default/end? scanner)
     :clj     (.isEnd ^PacoScanner scanner)
     :default (default/end? scanner)))

(defn peek
  #?(:clj {:inline (fn [scanner] `(.peek ~(tag scanner)))})
  [scanner]
  #?(:bb      (default/peek scanner)
     :clj     (.peek ^PacoScanner scanner)
     :default (default/peek scanner)))

(defn skip!
  #?(:clj {:inline (fn
                     ([scanner]
                      `(.skip ~(tag scanner)))
                     ([scanner n]
                      `(.skip ~(tag scanner) ~n)))})
  ([scanner]
   #?(:bb      (default/skip! scanner)
      :clj     (.skip ^PacoScanner scanner)
      :default (default/skip! scanner)))
  ([scanner n]
   #?(:bb      (default/skip! scanner n)
      :clj     (.skip ^PacoScanner scanner n)
      :default (default/skip! scanner n))))

(defn state
  #?(:clj {:inline (fn [scanner] `(.state ~(tag scanner)))})
  [scanner]
  #?(:bb      (default/state scanner)
     :clj     (.state ^PacoScanner scanner)
     :default (default/state scanner)))

(defn in-state?
  #?(:clj {:inline (fn [scanner state] `(.inState ~(tag scanner) ~state))})
  [scanner state]
  #?(:bb      (default/in-state? scanner state)
     :clj     (.inState ^PacoScanner scanner state)
     :default (default/in-state? scanner state)))

(defn backtrack!
  #?(:clj {:inline (fn [scanner state] `(.backtrack ~(tag scanner) ~state))})
  [scanner state]
  #?(:bb      (default/backtrack! scanner state)
     :clj     (.backtrack ^PacoScanner scanner state)
     :default (default/backtrack! scanner state)))

;; ICharScanner

(defn peek-str
  #?(:clj {:inline (fn [scanner n] `(.peekString ~(tag scanner) ~n))})
  [scanner n]
  #?(:bb      (default/peek-str scanner n)
     :clj     (.peekString ^PacoScanner scanner n)
     :default (default/peek-str scanner n)))

(defn read-str!
  #?(:clj {:inline (fn [scanner n] `(.readString ~(tag scanner) ~n))})
  [scanner n]
  #?(:bb      (default/read-str! scanner n)
     :clj     (.readString ^PacoScanner scanner n)
     :default (default/read-str! scanner n)))

(defn matches-char-pred?
  #?(:clj {:inline (fn [scanner pred]
                     `(.matches ~(tag scanner) ~(tag pred 'paco.detail.jvm.CharPredicate)))})
  [scanner pred]
  #?(:bb      (default/matches-char-pred? scanner pred)
     :clj     (.matches ^PacoScanner scanner ^CharPredicate pred)
     :default (default/matches-char-pred? scanner pred)))

(defn matches-str?
  #?(:clj {:inline (fn [scanner s] `(.matchesString ~(tag scanner) ~s))})
  [scanner s]
  #?(:bb      (default/matches-str? scanner s)
     :clj     (.matchesString ^PacoScanner scanner s)
     :default (default/matches-str? scanner s)))

(defn matches-str-ci?
  #?(:clj {:inline (fn [scanner s] `(.matchesStringIgnoreCase ~(tag scanner) ~s))})
  [scanner s]
  #?(:bb      (default/matches-str-ci? scanner s)
     :clj     (.matchesStringIgnoreCase ^PacoScanner scanner s)
     :default (default/matches-str-ci? scanner s)))

(defn re-match
  #?(:clj {:inline (fn [scanner re] `(.matchRegex ~(tag scanner) ~re))})
  [scanner re]
  #?(:bb      (default/re-match scanner re)
     :clj     (.matchRegex ^PacoScanner scanner re)
     :default (default/re-match scanner re)))

(defn re-groups [scanner re]
  (when-let [m (re-match scanner re)]
    #?(:clj  (clojure.core/re-groups m)
       :cljs (if (== (.-length m) 1)
               (aget m 0)
               (vec m)))))

(defn read-char-when!
  "Reads and returns the next character when it satisfies `pred`.  Returns
   `false` when it does not satsify `pred`, and `nil` at the end of the
   input stream."
  [scanner pred]
  #?(:bb      (default/read-char-when! scanner pred)
     :clj     (let [ch (.readCharWhen ^PacoScanner scanner ^CharPredicate pred)]
                (case ch -1 nil -2 false (char ch)))
     :default (default/read-char-when! scanner pred)))

(defn skip-chars-while!
  #?(:clj {:inline (fn [scanner pred]
                     `(.skipCharsWhile ~(tag scanner) ~(tag pred 'paco.detail.jvm.CharPredicate)))})
  [scanner pred]
  #?(:bb      (default/skip-chars-while! scanner pred)
     :clj     (.skipCharsWhile ^PacoScanner scanner ^CharPredicate pred)
     :default (default/skip-chars-while! scanner pred)))

(defn read-from
  #?(:clj {:inline (fn [scanner start] `(.readFrom ~(tag scanner) ~start))})
  [scanner start]
  #?(:bb      (default/read-from scanner start)
     :clj     (.readFrom ^PacoScanner scanner start)
     :default (default/read-from scanner start)))

;; IUserStateScanner

(defn modcount
  #?(:clj {:inline (fn [scanner] `(.modCount ~(tag scanner)))})
  [scanner]
  #?(:bb      (default/modcount scanner)
     :clj     (.modCount ^PacoScanner scanner)
     :default (default/modcount scanner)))

(defn backtrack-modified!
  #?(:clj {:inline (fn [scanner state] `(.backtrackModified ~(tag scanner) ~state))})
  [scanner state]
  #?(:bb      (default/backtrack-modified! scanner state)
     :clj     (.backtrackModified ^PacoScanner scanner state)
     :default (default/backtrack-modified! scanner state)))

(defn user-state
  #?(:clj {:inline (fn [scanner] `(.getUserState ~(tag scanner)))})
  [scanner]
  #?(:bb      (default/user-state scanner)
     :clj     (.getUserState ^PacoScanner scanner)
     :default (default/user-state scanner)))

(defn with-user-state!
  #?(:clj {:inline (fn [scanner state] `(doto ~(tag scanner) (.setUserState ~state)))})
  [scanner state]
  #?(:bb      (default/with-user-state! scanner state)
     :clj     (doto ^PacoScanner scanner (.setUserState state))
     :default (default/with-user-state! scanner state)))

;; ILineTrackingScanner

#?(:clj (let [int-mask (dec (bit-shift-left 1 32))]
          (defn -long->position [^long p]
            (pos/position (bit-shift-right p 32) (bit-and p int-mask)))))

(defn position
  #?(:clj {:inline (fn [scanner] `(-long->position (.position ~(tag scanner))))})
  [scanner]
  #?(:bb      (default/position scanner)
     :clj     (-long->position (.position ^PacoScanner scanner))
     :default (default/position scanner)))

(defn untracked-skip!
  #?(:clj {:inline (fn
                     ([scanner]
                      `(.untrackedSkip ~(tag scanner)))
                     ([scanner n]
                      `(.untrackedSkip ~(tag scanner) ~n)))})
  ([scanner]
   #?(:bb      (default/untracked-skip! scanner)
      :clj     (.untrackedSkip ^PacoScanner scanner)
      :default (default/untracked-skip! scanner)))
  ([scanner n]
   #?(:bb      (default/untracked-skip! scanner n)
      :clj     (.untrackedSkip ^PacoScanner scanner n)
      :default (default/untracked-skip! scanner n))))

(defn of
  ([input]
   (of input nil))
  ([input opts]
   #?(:bb      (default/of input opts)
      :clj     (PacoScanner/of ^String input (:user-state opts) (boolean (:line-tracking? opts true)))
      :default (default/of input opts))))
