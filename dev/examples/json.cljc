(ns examples.json
  (:refer-clojure :exclude [array])
  (:require [clojure.string :as str]
            [paco.char :as c]
            [paco.core :as p]
            [paco.detail.char-preds :as char-preds]
            [paco.detail.error :as error]
            [paco.detail.reply :as reply]
            [paco.detail.scanner :as scanner])
  #?(:clj (:import [clojure.lang MapEntry])))

;; TODO: skip-match
;; TODO: <??>, label fail! (aka, as)
(def whitespace
  (p/skip* (p/as (c/any-of " \r\n\t") "whitespace")))

(def slow-number
  (let [integer (c/strcat (p/? (c/char \-))
                          (p/alt (c/char \0)
                                 (p/cat (c/char-range \1 \9) (p/* c/digit))))
        fraction (p/? (c/strcat (c/char \.) (p/+ c/digit)))
        exponent (p/? (c/strcat (c/any-of "Ee")
                                (p/? (c/any-of "-+"))
                                (p/+ c/digit)))]
    (p/with [integer integer
             fraction fraction
             exponent exponent]
      (p/return
       (if (or fraction exponent)
         (parse-double (str integer fraction exponent))
         (parse-long integer))))))

(defn number [scanner reply]
  (if-let [m (scanner/re-match scanner #"-?(?:0|[1-9][0-9]*)(\.[0-9]+)?([eE][-+]?[0-9]+)?")]
    (let [m #?(:bb m, :clj ^java.util.regex.MatchResult m, :cljs m)
          s #?(:clj (.group m), :cljs (aget m 0))
          n #?(:clj (.length s), :cljs (.-length s))
          v (if (and #?(:clj (neg? (.start m 1)), :cljs (aget m 1))
                     #?(:clj (neg? (.start m 2)), :cljs (aget m 2)))
              ;; Only the integer part matched.
              #?(:clj (if (< n 18)
                        (Long/valueOf s)
                        (or (try (Long/valueOf s)
                                 (catch NumberFormatException _ nil))
                            (bigint s)))
                 :cljs (js/parseInt s 10))
              ;; Also the fraction and/or exponent matched
              #?(:clj (Double/valueOf s), :cljs (parse-double s)))]
      (scanner/skip! scanner n)
      (reply/ok reply v))
    (reply/fail reply (error/merge (error/unexpected-token-or-end scanner)
                                   (error/expected "number")))))

(defn- parse-int [s radix]
  #?(:clj  (Long/parseLong s radix)
     :cljs (js/parseInt s radix)))

(def string
  (-> (p/alt (-> (c/skip-char \\)
                 (p/then (p/alt (c/any-of "\"\\/")
                                (c/char-return \b \backspace)
                                (c/char-return \f \formfeed)
                                (c/char-return \n \newline)
                                (c/char-return \r \return)
                                (c/char-return \t \tab)
                                (-> (c/skip-char \u)
                                    (p/then (-> (p/repeat c/hex 4)
                                                (p/pipe (fn [chs]
                                                          (-> (str/join chs)
                                                              (parse-int 16)
                                                              char)))))))))
             (c/match #(not (or (#{\" \\} %)
                                (char-preds/control? %)))))
      c/str*
      (p/between (c/skip-char \"))))

(comment
  (p/parse string "\"\"")
  (p/parse string "\"foo\"")
  (p/parse string "\"line 1\\nline 2\"")
  ;;
  )

(defn comma-sep [p]
  (p/sep-by* p (c/skip-char \,)))

(declare value)

(def array
  (-> whitespace
      (p/then (comma-sep (p/lazy value)))
      (p/between (c/skip-char \[) (c/skip-char \]))))

(comment
  (p/parse array "[1, 2, 3, 4]")
  ;;
  )

(def object-entry
  (p/with [_ whitespace
           k string
           _ whitespace
           _ (c/skip-char \:)
           v value]
    (p/return (MapEntry. k v))))

(def object
  (p/with [_ (c/skip-char \{)
           _ whitespace
           entries (comma-sep object-entry)
           _ (c/skip-char \})]
    (p/return (into {} entries))))

(def value
  (let [value (p/alts [string
                       number
                       object
                       array
                       (c/string-return "true" true)
                       (c/string-return "false" false)
                       (c/string-return "null" nil)]
                      "value")]
    (p/with [_ whitespace
             v value
             _ whitespace]
      (p/return v))))

(def json (p/then-skip value p/end))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment
  (p/parse number "172")
  (p/parse number "172.23e-2")
  (p/parse string "\"foo\\u0044bar\"")
  (p/parse whitespace "   \r\n")

  (p/parse (comma-sep number) "")
  (p/parse (comma-sep number) "1,2,3,4")

  (p/parse string "19, \"")

  (p/parse (p/alts [string
                    number
                    object
                    array
                    (c/string-return "true" true)
                    (c/string-return "false" false)
                    (c/string-return "null" nil)]
                   "value")
           "19, \"")

  (p/parse object-entry "\"key\" : 19, \"bar\": false")
  (p/parse (comma-sep object-entry) "\"key\" : 19, \"bar\": false")

  (p/parse json "  ")
  (p/parse json "[1 2]")
  (p/parse (p/alt (p/attempt number) string) "1.")
  (p/parse json "19")
  (p/parse json "[1, 2, false, null]")
  (p/parse json "{\"foo\": 19, \"bar\" : false, \"x\": [\"y\"]}")

  (require '[clojure.data.json :as json])

  (def input (slurp "dev/examples/json/example4.json"))
  (def input (slurp "dev/experiments/citm_catalog.json"))

  (def us (p/parse json input))
  (def them (json/read-str input))

  (= us them)

  (require '[criterium.core :as criterium])

  ;; them
  (criterium/quick-bench
   (json/read-str input))
  ;; (Execution time mean : 27,387667 µs)
  ;; Execution time mean : 25,358191 ms

  ;; us
  (criterium/quick-bench
   (p/parse json input))
  ;; (Execution time mean : 595,354831 µs)
  ;; Execution time mean : 162,104478 ms

  (criterium/quick-bench
   (p/parse json input :line-tracking? false))
  ;; Execution time mean : 135,796054 ms

  ;;
  )
