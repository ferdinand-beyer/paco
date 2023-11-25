(ns bench.replies
  (:require [criterium.core :refer [bench]]))

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

(defn return-csp [x]
  (fn [state ok _ok! _error _error!]
    (ok state x (list :test))))

(defn fwd-ok [ok errors]
  (if (seq errors)
    (fn [s v e]
      (ok s v (concat errors e)))
    ok))

(defn fwd-error [error errors]
  (if (seq errors)
    (fn [s e]
      (error s (concat errors e)))
    error))

(defn bind-csp [p f]
  (fn [state ok ok! error error!]
    (fn []
      (p state
         (fn [s1 v1 e1]
           (fn []
             ((f v1) s1
                     (fwd-ok ok e1)
                     ok!
                     (fwd-error error e1)
                     error!)))
         (fn [s1 v1 e1]
           (fn []
             ((f v1) s1 (fwd-ok ok! e1) ok! error! error!)))
         error
         error!))))

(defn run-csp [p state]
  (letfn [(ok [s v e]
            (list :ok s v e))
          (error [s e]
            (list :error s e))]
    (trampoline p state ok ok error error)))

(comment
  (def input "The quick brown fox jumps over the lazy dog.")

  (bench
   (run-seq input))

  (bench
   (run-seq* input))

  (bench
   (run-stream input))

  (bench
   (run-stream* input))

  (def reply (ReplyFns. (constantly :ok)
                        (constantly :ok!)
                        (constantly :error)
                        (constantly :error!)))

  (bench
   (-ok! reply nil nil nil))

  (bench
   (.-ok! ^ReplyFns reply nil nil nil))

  (bench
   ((.-ok! ^ReplyFns reply) nil nil nil))

  (bench
   (run-csp (bind-csp (return-csp :foo)
                      (constantly (return-csp :bar)))
            ""))

  ;;
  )
