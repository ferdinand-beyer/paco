(ns examples.json
  (:refer-clojure :exclude [array])
  (:require [paco.core :as p]
            [paco.chars :as c]
            [clojure.string :as str])
  #?(:clj (:import [clojure.lang MapEntry])))

;; TODO: skip-match
;; TODO: <??>, label fail! (aka, as)
(def whitespace
  (p/skip* (p/as (c/any-of " \r\n\t") "whitespace")))

;; TODO: reduce to string (fparsec: manyChars, skipped)
(def number
  (p/with [integer (p/cat
                    (p/? (c/char \-))
                    (p/alt (c/char \0)
                           (p/cat (c/char-range \1 \9) (p/* c/digit))))
           fraction (p/? (p/cat (c/char \.) (p/+ c/digit)))
           exponent (p/? (p/cat (c/any-of "Ee")
                                (p/? (c/any-of "-+"))
                                (p/+ c/digit)))]
    (p/return
     (if (or fraction exponent)
       (->> (concat integer fraction exponent) str/join parse-double)
       (->> integer str/join parse-long)))))

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
                                (c/control? %)))))
      c/str*
      (p/between (c/skip-char \"))))

(comment
  (p/parse string "\"\"")
  (p/parse string "\"foo\"")
  (p/parse string "\"line 1\\nline 2\"")
  ;;
  )

(declare value)

(defn comma-sep [p]
  (p/sep-by* p (c/skip-char \,)))

(def array
  (-> whitespace
      (p/then (comma-sep value))
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
  (p/with [_ whitespace
           v (p/alts [string
                      number
                      object
                      array
                      (c/string-return "true" true)
                      (c/string-return "false" false)
                      (c/string-return "null" nil)]
                     "value")
           _ whitespace]
    (p/return v)))

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

  (def us (p/parse json input))
  (def them (json/read-str input))

  (= us them)

  (require '[criterium.core :as criterium])

  ;; them
  (criterium/quick-bench
   (json/read-str input))
  ;; Execution time mean : 27,387667 µs

  ;; us
  (criterium/quick-bench
   (p/parse json input))
  ;; Execution time mean : 595,354831 µs

  ;;
  )
