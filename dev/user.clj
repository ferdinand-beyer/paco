(ns user
  (:require [criterium.core :as c]))

(set! *warn-on-reflection* true)

(defprotocol IStream
  (-peek [stream])
  (-skip [stream]))

(deftype StringStream [^String s ^int pos]
  IStream
  (-peek [_] (.charAt s pos))
  (-skip [_] (StringStream. s (unchecked-inc pos))))

(comment
  (def input "The quick brown fox jumps over the lazy dog.")

  (let [s (seq input)]
    (c/bench (first (next (next s)))))

  (let [^clojure.lang.ISeq s (seq input)]
    (c/bench (.first (.next (.next s)))))

  (let [s (StringStream. input 0)]
    (c/bench (-peek (-skip (-skip s)))))

  (def stream (StringStream. input 0))
  (c/bench (-peek (-skip (-skip stream))))

  (let [s (StringStream. input 0)]
    (-peek (-skip (-skip s))))

  ;;
  )
