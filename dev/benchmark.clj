(ns benchmark
  (:require [criterium.core :as criterium]))

(set! *warn-on-reflection* true)

(defn run-seq [string]
  (loop [s (seq string)
         i 0]
    (if (first s)
      (recur (next s) (inc i))
      i)))

(defn run-seq* [string]
  (loop [^clojure.lang.ISeq s (seq string)
         i 0]
    (if (and s (.first s))
      (recur (.next s) (inc i))
      i)))

(defprotocol ICharStream
  (peek-char [_])
  (skip-char [_]))

(deftype EndStream []
  ICharStream
  (peek-char [_] nil)
  (skip-char [this] this))

(deftype StringCharStream [^String s ^int index]
  ICharStream
  (peek-char [_] (.charAt s index))
  (skip-char [_]
    (let [new-index (unchecked-inc index)]
      (if (< new-index (.length s))
        (StringCharStream. s new-index)
        (EndStream.)))))

(defn run-stream [string]
  (loop [s (StringCharStream. string 0)
         i 0]
    (if (peek-char s)
      (recur (skip-char s) (inc i))
      i)))

(defn run-stream* [string]
  (loop [^benchmark.ICharStream s (StringCharStream. string 0)
         i 0]
    (if (.peek-char s)
      (recur (.skip-char s) (inc i))
      i)))

(defprotocol IReply
  (-ok [reply state result errors])
  (-ok! [reply state result errors])
  (-error [reply state errors])
  (-error! [reply state errors]))

(deftype ReplyFns [ok ok! error error!]
  IReply
  (-ok [_ state result errors] (ok state result errors))
  (-ok! [_ state result errors] (ok! state result errors))
  (-error [_ state errors] (error state errors))
  (-error! [_ state errors] (error! state errors)))

(comment
  (def input "The quick brown fox jumps over the lazy dog.")

  (criterium/bench
   (run-seq input))

  (criterium/bench
   (run-seq* input))

  (criterium/bench
   (run-stream input))

  (criterium/bench
   (run-stream* input))

  (def reply (ReplyFns. (constantly :ok)
                        (constantly :ok!)
                        (constantly :error)
                        (constantly :error!)))

  (criterium/bench
   (-ok! reply nil nil nil))

  (criterium/bench
   (.-ok! ^ReplyFns reply nil nil nil))

  (criterium/bench
   ((.-ok! ^ReplyFns reply) nil nil nil))

  ;;
  )
