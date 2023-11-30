(ns bench.showdown
  (:require [blancas.kern.core :as k]
            [paco.chars :as c]
            [paco.core :as p]
            [criterium.core :as criterium]
            [strojure.parsesso.char :as qc]
            [strojure.parsesso.parser :as q]
            [the.parsatron :as t]))

(set! *warn-on-reflection* true)

(def ^:dynamic *mode* :off)

(comment
  (alter-var-root #'*mode* (constantly :eval))
  (alter-var-root #'*mode* (constantly :bench))
  (alter-var-root #'*mode* (constantly :quick))
  ;;
  )

(defmacro bench [& body]
  `(case *mode*
     :off nil
     :bench (criterium/bench ~@body)
     :quick (criterium/quick-bench ~@body)
     (do ~@body)))

(defmacro bench-paco [parser input]
  `(let [parser# ~parser
         input#  ~input]
     (bench (p/parse parser# input#))))

(defmacro bench-parsesso [parser input]
  `(let [parser# ~parser
         input#  ~input]
     (bench (q/parse parser# input#))))

(defmacro bench-kern [parser input]
  `(let [parser# ~parser
         input#  ~input]
     (bench (k/parse parser# input#))))

(defmacro bench-parsatron [parser input]
  `(let [parser# ~parser
         input#  ~input]
     (bench (t/run parser# input#))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:private -input-10000 (apply str (repeat 10000 \a)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Return value without parsing ##

(bench-paco (p/return :x) "")
;             Execution time mean : 18,306604 ns
;    Execution time std-deviation : 1,920469 ns
;   Execution time lower quantile : 17,362660 ns ( 2,5%)
;   Execution time upper quantile : 21,622863 ns (97,5%)

(bench-parsesso (q/result :x) "")
;             Execution time mean : 64,838090 ns
;    Execution time std-deviation : 4,645637 ns
;   Execution time lower quantile : 61,918959 ns ( 2,5%)
;   Execution time upper quantile : 72,538383 ns (97,5%)

(bench-kern (k/return :x) "")
;             Execution time mean : 147,719788 ns
;    Execution time std-deviation : 12,884972 ns
;   Execution time lower quantile : 138,992580 ns ( 2,5%)
;   Execution time upper quantile : 168,537118 ns (97,5%)

(bench-parsatron (t/always :x) "")
;             Execution time mean : 101,795612 ns
;    Execution time std-deviation : 6,134768 ns
;   Execution time lower quantile : 98,512050 ns ( 2,5%)
;   Execution time upper quantile : 112,373480 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Fail immediately without parsing ##

(bench (p/run (p/fail :x) ""))
;             Execution time mean : 44,989624 ns
;    Execution time std-deviation : 0,262919 ns
;   Execution time lower quantile : 44,768464 ns ( 2,5%)
;   Execution time upper quantile : 45,426122 ns (97,5%)

(bench (q/parse* (q/fail :x) []))
;             Execution time mean : 81,478078 ns
;    Execution time std-deviation : 0,673121 ns
;   Execution time lower quantile : 80,699739 ns ( 2,5%)
;   Execution time upper quantile : 82,937188 ns (97,5%)

(bench-kern (k/fail :x) [])
;             Execution time mean : 386,590746 ns
;    Execution time std-deviation : 156,097460 ns
;   Execution time lower quantile : 266,519628 ns ( 2,5%)
;   Execution time upper quantile : 640,785168 ns (97,5%)

(bench
 (t/run-parser (t/never) (t/->InputState [] (t/->SourcePos 1 1))))
;             Execution time mean : 841,250545 ns
;    Execution time std-deviation : 206,671857 ns
;   Execution time lower quantile : 703,388694 ns ( 2,5%)
;   Execution time upper quantile : 1,115857 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse token ##

(bench-paco (c/match #(= \a %)) "abc")
;             Execution time mean : 70,992962 ns
;    Execution time std-deviation : 4,885139 ns
;   Execution time lower quantile : 67,366876 ns ( 2,5%)
;   Execution time upper quantile : 76,592771 ns (97,5%)
;                   Overhead used : 1,924675 ns

(bench-parsesso (q/token #(= \a %)) "abc")
;             Execution time mean : 93,684178 ns
;    Execution time std-deviation : 6,080481 ns
;   Execution time lower quantile : 89,362737 ns ( 2,5%)
;   Execution time upper quantile : 104,113977 ns (97,5%)

(bench-kern (k/satisfy #(= \a %)) "abc")
;             Execution time mean : 245,984170 ns
;    Execution time std-deviation : 13,553994 ns
;   Execution time lower quantile : 235,005603 ns ( 2,5%)
;   Execution time upper quantile : 268,329750 ns (97,5%)

(bench-parsatron (t/token #(= \a %)) "abc")
;             Execution time mean : 557,024259 ns
;    Execution time std-deviation : 14,359373 ns
;   Execution time lower quantile : 541,631508 ns ( 2,5%)
;   Execution time upper quantile : 578,875966 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse word ##

(bench-paco (c/string "abc") "abc")
;             Execution time mean : 37,489351 ns
;    Execution time std-deviation : 3,434824 ns
;   Execution time lower quantile : 35,345999 ns ( 2,5%)
;   Execution time upper quantile : 43,220018 ns (97,5%)

(bench-parsesso (q/word "abc") "abc")
;             Execution time mean : 143,127655 ns
;    Execution time std-deviation : 9,777534 ns
;   Execution time lower quantile : 135,603025 ns ( 2,5%)
;   Execution time upper quantile : 159,714481 ns (97,5%)

(bench-kern (k/token* "abc") "abc")
;             Execution time mean : 4,020720 µs
;    Execution time std-deviation : 429,048420 ns
;   Execution time lower quantile : 3,767589 µs ( 2,5%)
;   Execution time upper quantile : 4,754242 µs (97,5%)

(bench-parsatron (t/string "abc") "abc")
;             Execution time mean : 2,212562 µs
;    Execution time std-deviation : 91,094400 ns
;   Execution time lower quantile : 2,126279 µs ( 2,5%)
;   Execution time upper quantile : 2,342896 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse word, case-insensitive ##

(bench-paco (c/string-ic "abc") "ABC")
;             Execution time mean : 48,778929 ns
;    Execution time std-deviation : 4,791800 ns
;   Execution time lower quantile : 45,219154 ns ( 2,5%)
;   Execution time upper quantile : 55,343194 ns (97,5%)

(bench-parsesso (q/word "abc" :ic) "ABC")
;             Execution time mean : 150,456878 ns
;    Execution time std-deviation : 12,496316 ns
;   Execution time lower quantile : 143,056370 ns ( 2,5%)
;   Execution time upper quantile : 171,845305 ns (97,5%)

(bench-kern (k/token- "abc") "ABC")
;             Execution time mean : 1,779934 µs
;    Execution time std-deviation : 116,763280 ns
;   Execution time lower quantile : 1,695811 µs ( 2,5%)
;   Execution time upper quantile : 1,938093 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse long word ##

(bench-paco (c/string -input-10000) -input-10000)
;             Execution time mean : 10,867368 µs
;    Execution time std-deviation : 87,342212 ns
;   Execution time lower quantile : 10,773234 µs ( 2,5%)
;   Execution time upper quantile : 10,999538 µs (97,5%)

(bench-parsesso (q/word -input-10000) -input-10000)
;             Execution time mean : 462,584681 µs
;    Execution time std-deviation : 17,856160 µs
;   Execution time lower quantile : 444,209929 µs ( 2,5%)
;   Execution time upper quantile : 489,421964 µs (97,5%)

(comment
  (bench-kern (k/token* -input-10000) -input-10000))
; Execution error (StackOverflowError) at blancas.kern.core/>>=$fn

(bench-parsatron (t/string -input-10000) -input-10000)
;             Execution time mean : 816,628230 µs
;    Execution time std-deviation : 16,412541 µs
;   Execution time lower quantile : 793,746125 µs ( 2,5%)
;   Execution time upper quantile : 831,238313 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse letters ##

(bench-parsesso (q/*many qc/letter?) "abc")
;             Execution time mean : 975,326535 ns
;    Execution time std-deviation : 65,828611 ns
;   Execution time lower quantile : 915,594047 ns ( 2,5%)
;   Execution time upper quantile : 1,059000 µs (97,5%)

(bench-kern (k/many k/letter) "abc")
;             Execution time mean : 1,911586 µs
;    Execution time std-deviation : 511,124107 ns
;   Execution time lower quantile : 1,646502 µs ( 2,5%)
;   Execution time upper quantile : 2,783604 µs (97,5%)

(bench-parsatron (t/many (t/letter)) "abc")
;             Execution time mean : 2,599675 µs
;    Execution time std-deviation : 576,904794 ns
;   Execution time lower quantile : 2,193151 µs ( 2,5%)
;   Execution time upper quantile : 3,354449 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse letters as string ##

(bench-parsesso (-> (q/*many qc/letter?) (q/value qc/str*)) "abc")
;             Execution time mean : 1,514160 µs
;    Execution time std-deviation : 104,898493 ns
;   Execution time lower quantile : 1,439323 µs ( 2,5%)
;   Execution time upper quantile : 1,680704 µs (97,5%)

(bench-kern (k/<+> (k/many k/letter)) "abc")
;             Execution time mean : 5,568215 µs
;    Execution time std-deviation : 145,037838 ns
;   Execution time lower quantile : 5,459951 µs ( 2,5%)
;   Execution time upper quantile : 5,810555 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Parse `many` for long input ##

(bench-paco (p/* (c/match #(= \a %))) -input-10000)
;             Execution time mean : 356,890924 µs
;    Execution time std-deviation : 5,169162 µs
;   Execution time lower quantile : 351,230010 µs ( 2,5%)
;   Execution time upper quantile : 365,182280 µs (97,5%)

(bench-parsesso (q/*many (q/token #(= \a %))) -input-10000)
;             Execution time mean : 490,252562 µs
;    Execution time std-deviation : 16,447039 µs
;   Execution time lower quantile : 475,693254 µs ( 2,5%)
;   Execution time upper quantile : 516,242197 µs (97,5%)

(bench-kern (k/many (k/satisfy #(= \a %))) -input-10000)
;             Execution time mean : 564,257691 µs
;    Execution time std-deviation : 19,430788 µs
;   Execution time lower quantile : 548,542033 µs ( 2,5%)
;   Execution time upper quantile : 591,819104 µs (97,5%)

(bench-parsatron (t/many (t/token #(= \a %))) -input-10000)
;             Execution time mean : 470,264471 ms
;    Execution time std-deviation : 21,226242 ms
;   Execution time lower quantile : 444,733040 ms ( 2,5%)
;   Execution time upper quantile : 492,588171 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Skip `many` for long input ##

(bench-parsesso (q/*skip (q/token #(= \a %))) -input-10000)
;             Execution time mean : 365,390438 µs
;    Execution time std-deviation : 3,226478 µs
;   Execution time lower quantile : 361,857830 µs ( 2,5%)
;   Execution time upper quantile : 370,005512 µs (97,5%)

(bench-kern (k/skip-many (k/satisfy #(= :a %))) -input-10000)
;             Execution time mean : 1,416146 ms
;    Execution time std-deviation : 35,717820 µs
;   Execution time lower quantile : 1,379739 ms ( 2,5%)
;   Execution time upper quantile : 1,451345 ms (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## The `alt` combinator ##

(bench-paco (p/alt (p/fail "a")
                       (p/fail "b")
                       (p/return :x)) "")
;             Execution time mean : 141,159754 ns
;    Execution time std-deviation : 4,565528 ns
;   Execution time lower quantile : 137,215126 ns ( 2,5%)
;   Execution time upper quantile : 147,166788 ns (97,5%)

(bench-parsesso (q/alt (q/fail "a")
                       (q/fail "b")
                       (q/result :x)) "")
;             Execution time mean : 177,446659 ns
;    Execution time std-deviation : 1,897155 ns
;   Execution time lower quantile : 175,001796 ns ( 2,5%)
;   Execution time upper quantile : 179,746675 ns (97,5%)

(bench-kern (k/<|> (k/fail "a")
                   (k/fail "b")
                   (k/return :x)) [])
;             Execution time mean : 830,268655 ns
;    Execution time std-deviation : 49,865394 ns
;   Execution time lower quantile : 779,447277 ns ( 2,5%)
;   Execution time upper quantile : 878,426631 ns (97,5%)

(bench-parsatron (t/choice (t/never)
                           (t/never)
                           (t/always :x)) [])
;             Execution time mean : 188,759986 ns
;    Execution time std-deviation : 2,111023 ns
;   Execution time lower quantile : 185,659380 ns ( 2,5%)
;   Execution time upper quantile : 190,389459 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Wrap with `expecting` ##

(bench-paco (-> (p/return :x) (p/expected "x")) "")
;             Execution time mean : 78,954855 ns
;    Execution time std-deviation : 7,143476 ns
;   Execution time lower quantile : 74,021075 ns ( 2,5%)
;   Execution time upper quantile : 89,549045 ns (97,5%)

(bench-parsesso (-> (q/result :x) (q/expecting "x")) [])
;             Execution time mean : 88,435374 ns
;    Execution time std-deviation : 6,137121 ns
;   Execution time lower quantile : 84,140302 ns ( 2,5%)
;   Execution time upper quantile : 98,433700 ns (97,5%)

(bench-kern (k/<?> (k/return :x) "x") [])
;             Execution time mean : 174,550774 ns
;    Execution time std-deviation : 14,574415 ns
;   Execution time lower quantile : 161,598259 ns ( 2,5%)
;   Execution time upper quantile : 192,160519 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ## Test for the end of input ##

(bench (q/parse* q/eof " "))
;             Execution time mean : 231,661354 ns
;    Execution time std-deviation : 25,008376 ns
;   Execution time lower quantile : 209,952763 ns ( 2,5%)
;   Execution time upper quantile : 262,847436 ns (97,5%)

(bench-kern k/eof " ")
;             Execution time mean : 1,428015 µs
;    Execution time std-deviation : 81,057937 ns
;   Execution time lower quantile : 1,352623 µs ( 2,5%)
;   Execution time upper quantile : 1,560179 µs (97,5%)

(bench
 (t/run-parser (t/eof) (t/->InputState " " (t/->SourcePos 1 1))))
;             Execution time mean : 882,705676 ns
;    Execution time std-deviation : 46,738939 ns
;   Execution time lower quantile : 837,580307 ns ( 2,5%)
;   Execution time upper quantile : 948,317437 ns (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(bench-parsesso (q/after (q/word "<!--")
                         (q/*many-till q/any-token (q/maybe (q/word "-->"))))
                "<!-- comment -->")
;             Execution time mean : 7,450434 µs
;    Execution time std-deviation : 607,080144 ns
;   Execution time lower quantile : 6,900613 µs ( 2,5%)
;   Execution time upper quantile : 8,221736 µs (97,5%)

(bench-kern (k/>> (k/token* "<!--")
                  (k/many-till k/any-char (k/<:> (k/token* "-->"))))
            "<!-- comment -->")
;             Execution time mean : 84,653453 µs
;    Execution time std-deviation : 2,870985 µs
;   Execution time lower quantile : 81,222728 µs ( 2,5%)
;   Execution time upper quantile : 87,938498 µs (97,5%)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
