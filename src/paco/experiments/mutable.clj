(ns paco.experiments.mutable
  (:require [paco.detail :as detail]
            [paco.error :as error])
  (:import [clojure.lang MapEntry]
           [java.util.regex MatchResult]
           [paco.impl
            CharPredicate
            ICharScanner
            IScanner
            StringScanner]))

;; This experiment explores implementing parsers in a
;; direct style, without CPS, and with a mutable reply
;; structure.  This is similar to fparsec.

;; Hypothesis: We can gain quite some performance by
;; avoiding allocation.

(set! *warn-on-reflection* true)

;; Leaf parsers receive an object satisfying this protocol.
;; They are supposed to report their result in tail position,
;; returning the reply's return value.
(defprotocol IReplyFactory
  (ok [self value] [self value error])
  (fail [self error])
  (fatal [self error]))

;; This protocol offers inspection of a reply structure.
(defprotocol IReply
  (status [reply])
  (value [reply])
  (error [reply])

  (with-status [reply status])
  (with-value [reply value])
  (with-error [reply error]))

(defn ok? [reply]
  (identical? :paco/ok (status reply)))

(defn not-ok? [reply]
  (not (identical? :paco/ok (status reply))))

(defn error? [reply]
  (identical? :paco/error (status reply)))

(defn fatal? [reply]
  (identical? :paco/fatal (status reply)))

(defn with-ok [reply]
  (with-status reply :paco/ok))

(deftype MutableReply [^:unsynchronized-mutable status*
                       ^:unsynchronized-mutable value*
                       ^:unsynchronized-mutable error*]
  IReply
  (status [_] status*)
  (value [_] value*)
  (error [_] error*)

  (with-status [self status]
    (set! status* status)
    self)
  (with-value [self value]
    (set! value* value)
    self)
  (with-error [self error]
    (set! error* error)
    self)

  IReplyFactory
  (ok [self value]
    (set! status* :paco/ok)
    (set! value* value)
    (set! error* nil)
    self)
  (ok [self value error]
    (set! status* :paco/ok)
    (set! value* value)
    (set! error* error)
    self)
  (fail [self error]
    (set! status* :paco/error)
    (set! value* nil)
    (set! error* error)
    self)
  (fatal [self error]
    (set! status* :paco/fatal)
    (set! value* nil)
    (set! error* error)
    self))

;; Parsers

(defn eof [^IScanner scanner reply]
  (if (.atEnd scanner)
    (ok reply nil)
    (fail reply (error/merge error/expected-end
                             (error/unexpected-input (.peekToken scanner))))))

(defn- unexpected [^IScanner scanner error]
  (error/merge error (if (.atEnd scanner)
                       error/unexpected-end
                       (error/unexpected-input (.peekToken scanner)))))

(defn char-return [ch value]
  (let [pred (CharPredicate/equals ch)
        err  (error/expected-input ch)]
    (fn [^ICharScanner scanner reply]
      (if (.matches scanner pred)
        (do
          (.skip scanner)
          (ok reply value))
        (fail reply (unexpected scanner err))))))

(defn skip-char [ch]
  (char-return ch nil))

(defn match-char [^CharPredicate pred label]
  (let [err (error/expected label)]
    (fn [^ICharScanner scanner reply]
      (if (.matches scanner pred)
        (let [token (.peekToken scanner)]
          (.skip scanner)
          (ok reply token))
        (fail reply (unexpected scanner err))))))

(defn any-of [^String chars]
  (match-char (CharPredicate/among chars) chars))

(defn string-return [^String s value]
  (let [err (error/expected-input s)]
    (fn [^ICharScanner scanner reply]
      (if (.matchesString scanner s)
        (do
          (.skip scanner (.length s))
          (ok reply value))
        (fail reply (unexpected scanner err))))))

(defn skip-whitespace [^ICharScanner scanner reply]
  (.skipCharsWhile scanner CharPredicate/WHITESPACE)
  (ok reply nil))

(defn number [^ICharScanner scanner reply]
  (if-let [^MatchResult m (.match scanner #"^-?(?:0|[1-9][0-9]*)(\.[0-9]+)?([eE][-+]?[0-9]+)?")]
    (let [s (.group m)
          n (.length s)
          v (if (and (neg? (.start m 1))
                     (neg? (.start m 2)))
              ;; Only the integer part matched.
              (if (< n 18)
                (Long/valueOf s)
                (or (try (Long/valueOf s)
                         (catch NumberFormatException _ nil))
                    (bigint s)))
              ;; Also the fraction and/or exponent matched
              (Double/valueOf s))]
      (.skip scanner n)
      (ok reply v))
    (fail reply (unexpected scanner (error/expected "number")))))

;; Combinators

(defn pipe
  ([p f]
   (fn [^IScanner scanner reply]
     (let [reply (p scanner reply)]
       (if (ok? reply)
         (with-value reply (f (value reply)))
         reply))))
  ([p1 p2 f]
   (fn [^IScanner scanner reply]
     (let [reply (p1 scanner reply)]
       (if (ok? reply)
         (let [value1 (value reply)
               reply  (if-some [error1 (error reply)]
                        (let [index1 (.index scanner)
                              reply  (p2 scanner reply)]
                          (if (= index1 (.index scanner))
                            (with-error reply (error/merge error1 (error reply)))
                            reply))
                        (p2 scanner reply))]
           (if (ok? reply)
             (with-value reply (f value1 (value reply)))
             reply))
         reply)))))

(defn then2 [p1 p2]
  (pipe p1 p2 (fn [_ x] x)))

(defn then
  ([p1 p2] (then2 p1 p2))
  ([p1 p2 p3 & more]
   (reduce then2 p1 (list* p2 p3 more))))

(defn alt2 [p1 p2]
  (fn [^IScanner scanner reply]
    (let [index0 (.index scanner)
          reply  (p1 scanner reply)]
      (if (and (error? reply)
               (= index0 (.index scanner)))
        (let [error1 (error reply)
              reply  (p2 scanner reply)]
          (if (= index0 (.index scanner))
            (with-error reply (error/merge error1 (error reply)))
            reply))
        reply))))

(defn alt
  ([p1 p2] (alt2 p1 p2))
  ([p1 p2 p3 & more]
   (reduce alt2 p1 (list* p2 p3 more))))

(defn opt [p]
  (fn [^IScanner scanner reply]
    (let [index (.index scanner)
          reply (p scanner reply)]
      (if (and (error? reply)
               (= index (.index scanner)))
        (with-ok reply)
        reply))))

(defn many [rf p]
  (fn [^IScanner scanner reply]
    (loop [acc    (rf)
           index0 (.index scanner)
           error0 nil]
      (let [reply  (p scanner reply)
            index1 (.index scanner)]
        (if (ok? reply)
          (if (= index0 index1)
            (throw (ex-info "infinite loop" {}))
            (recur (rf acc (value reply))
                   index1
                   (error reply)))
          (if (error? reply)
            (let [error1 (error reply)]
              (ok reply
                  (rf acc)
                  (if (and error0 (= index0 index1))
                    (error/merge error0 error1)
                    error1)))
            ;; fatal
            reply))))))

(defn then-skip2 [p q]
  (pipe p q (fn [x _] x)))

(defn between [p open close]
  (then open (then-skip2 p close)))

(defn sep-by [p sep]
  (-> (pipe p (many detail/seqexp-rf (then sep p)) cons)
      opt
      (pipe (fn [s] (or s [])))))

;; Composite parsers


(def string
  (let [q (skip-char \")
        u (let [h (match-char CharPredicate/HEX "hex digit")
                p (pipe h h list)]
            (pipe p p (fn [[a b] [c d]]
                        (-> (str a b c d)
                            (Long/parseLong 16)
                            char))))
        c (match-char (CharPredicate/not
                       (CharPredicate/or
                        (CharPredicate/among "\"\\")
                        CharPredicate/ISO_CONTROL))
                      "char")
        e (then (skip-char \\)
                (alt (any-of "\"\\/")
                     (char-return \b \backspace)
                     (char-return \f \formfeed)
                     (char-return \n \newline)
                     (char-return \r \return)
                     (char-return \t \tab)
                     (then (skip-char \u) u)))
        s (many detail/string-rf (alt e c))]
    (between s q q)))

(defn comma-sep [p]
  (sep-by p (skip-char \,)))

(declare json-value)

(defmacro fwd [p]
  `(fn [scanner# reply#]
     (~p scanner# reply#)))

(def array
  (-> skip-whitespace
      (then (comma-sep (fwd json-value)))
      (between (skip-char \[) (skip-char \]))))

(def object-entry
  (pipe (then skip-whitespace string)
        (then skip-whitespace (skip-char \:) (fwd json-value))
        (fn [k v]
          (MapEntry. k v))))

(def object
  (-> (pipe skip-whitespace (comma-sep object-entry)
            (fn [_ entries]
              (into {} entries)))
      (between (skip-char \{) (skip-char \}))))

(def json-value
  (-> (alt string
           number
           object
           array
           (string-return "true" true)
           (string-return "false" false)
           (string-return "null" nil))
      (between skip-whitespace skip-whitespace)))

(def json (then-skip2 json-value eof))

(defn parse [p input]
  (let [scanner (StringScanner. input)
        reply (p scanner (MutableReply. nil nil nil))]
    (if (ok? reply)
      (value reply)
      (throw (ex-info (str (.index scanner) ": "
                           (error/string (error reply))) {})))))

(comment
  (parse eof "foo")
  (parse skip-whitespace "   ")

  (parse number "1e7")

  (parse (then (comma-sep number) eof) "1.23123,")

  (parse number "1.234   ")
  (parse (pipe skip-whitespace number vector) "   45")
  (parse (then skip-whitespace number skip-whitespace number) " 1 2")

  (parse number "18123712938712983791827371239   ")

  (parse (match-char CharPredicate/HEX "hex") "g")

  (parse json-value "17")
  (parse json "[1, 2, 3]")
  (parse json "{\"a\": 17, \"b\": 18}")
  (parse json "{}")
  (parse json "[]")

  (require '[criterium.core :as criterium])
  (def input (slurp "dev/experiments/citm_catalog.json"))

  (criterium/quick-bench
   (parse json input))
  ;; Execution time mean : 39,345940 ms
  ;; data.json: ~25ms
  ;; paco/cps: ~180ms

  (require '[clojure.data.json :as json])

  (def us (parse json input))
  (def them (json/read-str input))

  (= us them)

  ;;
  )
