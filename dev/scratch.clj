(ns scratch
  (:require [criterium.core :as criterium]))

(set! *warn-on-reflection* true)

(deftype StringView [^CharSequence cs ^int start ^int end]
  CharSequence
  (length [_]
    (unchecked-subtract-int end start))
  (charAt [_ index]
    (.charAt cs (unchecked-add-int start index)))
  (subSequence [_ s e]
    (StringView. cs (unchecked-add-int start s) (unchecked-add-int start e)))
  (toString [_]
    (.. cs (subSequence start end) toString)))

(comment
  (def ^String source "The quick brown fox jumps over the lazy dog.")

  (criterium/quick-bench
   (.regionMatches source 10 "brown fox" 0 9))

  (let [v (StringView. source 10 19)]
    (criterium/quick-bench
     (CharSequence/compare v "brown fox")))


  )
