(ns paco.detail.scanner
  (:refer-clojure :exclude [peek re-groups])
  (:require #?(:bb   [paco.detail.scanner.default :as default]
               :cljs [paco.detail.scanner.default :as default])
            #?(:clj [paco.detail.position :as pos]))
  #?@(:bb  []
      :clj [(:import [paco.detail.jvm CharPredicate PacoScanner])]))

#?(:clj (set! *warn-on-reflection* true))

;; IScanner

(defn index [scanner]
  #?(:bb      (default/index scanner)
     :clj     (.index ^PacoScanner scanner)
     :default (default/index scanner)))

(defn end? [scanner]
  #?(:bb      (default/end? scanner)
     :clj     (.isEnd ^PacoScanner scanner)
     :default (default/end? scanner)))

(defn peek [scanner]
  #?(:bb      (default/peek scanner)
     :clj     (.peek ^PacoScanner scanner)
     :default (default/peek scanner)))

(defn matches? [scanner pred]
  #?(:bb      (default/matches? scanner pred)
     :clj     (.matches ^PacoScanner scanner (CharPredicate/of pred))
     :default (default/matches? scanner pred)))

(defn skip!
  ([scanner]
   #?(:bb      (default/skip! scanner)
      :clj     (.skip ^PacoScanner scanner)
      :default (default/skip! scanner)))
  ([scanner n]
   #?(:bb      (default/skip! scanner n)
      :clj     (.skip ^PacoScanner scanner n)
      :default (default/skip! scanner n))))

(defn state [scanner]
  #?(:bb      (default/state scanner)
     :clj     (.state ^PacoScanner scanner)
     :default (default/state scanner)))

(defn in-state? [scanner state]
  #?(:bb      (default/in-state? scanner state)
     :clj     (.inState ^PacoScanner scanner state)
     :default (default/in-state? scanner state)))

(defn backtrack! [scanner state]
  #?(:bb      (default/backtrack! scanner state)
     :clj     (.backtrack ^PacoScanner scanner state)
     :default (default/backtrack! scanner state)))

;; ICharScanner

(defn peek-str [scanner n]
  #?(:bb      (default/peek-str scanner n)
     :clj     (.peekString ^PacoScanner scanner n)
     :default (default/peek-str scanner n)))

(defn read-str [scanner n]
  #?(:bb      (default/read-str scanner n)
     :clj     (.readString ^PacoScanner scanner n)
     :default (default/read-str scanner n)))

(defn matches-str? [scanner s]
  #?(:bb      (default/matches-str? scanner s)
     :clj     (.matchesString ^PacoScanner scanner s)
     :default (default/matches-str? scanner s)))

(defn matches-str-ci? [scanner s]
  #?(:bb      (default/matches-str-ci? scanner s)
     :clj     (.matchesStringIgnoreCase ^PacoScanner scanner s)
     :default (default/matches-str-ci? scanner s)))

(defn re-match [scanner re]
  #?(:bb      (default/re-match scanner re)
     :clj     (.matchRegex ^PacoScanner scanner re)
     :default (default/re-match scanner re)))

(defn read-from [scanner start]
  #?(:bb      (default/read-from scanner start)
     :clj     (.readFrom ^PacoScanner scanner start)
     :default (default/read-from scanner start)))

(defn re-groups [scanner re]
  (when-let [m (re-match scanner re)]
    #?(:clj  (clojure.core/re-groups m)
       :cljs (if (== (.-length m) 1)
               (aget m 0)
               (vec m)))))

;; IUserStateScanner

(defn modcount [scanner]
  #?(:bb      (default/modcount scanner)
     :clj     (.modCount ^PacoScanner scanner)
     :default (default/modcount scanner)))

(defn backtrack-modified! [scanner state]
  #?(:bb      (default/backtrack-modified! scanner state)
     :clj     (.backtrackModified ^PacoScanner scanner state)
     :default (default/backtrack-modified! scanner state)))

(defn user-state [scanner]
  #?(:bb      (default/user-state scanner)
     :clj     (.getUserState ^PacoScanner scanner)
     :default (default/user-state scanner)))

(defn with-user-state! [scanner state]
  #?(:bb      (default/with-user-state! scanner state)
     :clj     (doto ^PacoScanner scanner (.setUserState state))
     :default (default/with-user-state! scanner state)))

;; ILineTrackingScanner

#?(:clj (let [int-mask (dec (bit-shift-left 1 32))]
          (defn- long->position [^long p]
            (pos/position (bit-shift-right p 32) (bit-and p int-mask)))))

(defn position [scanner]
  #?(:bb      (default/position scanner)
     :clj     (long->position (.position ^PacoScanner scanner))
     :default (default/position scanner)))

(defn untracked-skip!
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

(comment
  (def s (of "Foo\nbar"))

  s
  (position s)
  (skip! s)
  (skip! s 17)
  (index s)
  (position s)
  (.position s)


  ;;
  )
