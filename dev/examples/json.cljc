(ns examples.json
  (:refer-clojure :exclude [array])
  (:require [paco.core :as p]
            [paco.chars :as c]
            [clojure.string :as str]))

;; TODO: skip-match
;; TODO: <??>, label fail! (aka, as)
(def whitespace
  (p/* (p/as (c/any-of " \r\n\t") "whitespace")))

;; TODO: reduce to string (fparsec: manyChars, skipped)
;; TODO: (chars/range \0 \9)
(def number
  (p/with [integer (p/cat
                    (p/? (c/char \-))
                    (p/alt (c/char \0)
                           (p/cat (c/any-of "123456789")
                                  (p/* c/digit))))
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

;; TODO: c/many-string
(def string
  (-> (p/* (p/alt (p/then (c/skip-char \\)
                          (p/alt (c/any-of "\"\\/")
                                 (c/char-return \b \backspace)
                                 (c/char-return \f \formfeed)
                                 (c/char-return \n \newline)
                                 (c/char-return \r \return)
                                 (c/char-return \t \tab)
                                 (p/then (c/skip-char \u)
                                         (p/pipe
                                          (p/repeat c/hex 4)
                                          (fn [chs]
                                            (-> (str/join chs)
                                                (parse-int 16)
                                                char))))))
                  (c/match #(not (or (#{\" \\} %)
                                     (c/control? %))))))
      (p/pipe str/join)
      (p/between (c/skip-char \"))))

(comment
  (p/parse string "\"\"")
  (p/parse string "\"foo\"")
  (p/parse string "\"line 1\\nline 2\"")
  ;;
  )

(declare value)

(def ^:private skip-comma (c/skip-char \,))

;; TODO: p/sep-by
(defn comma-sep [p]
  (p/? (p/with [x p
                xs (p/* (p/then skip-comma p))]
         (p/return (cons x xs)))))

(def array
  (p/with [_ (c/skip-char \[)
           _ whitespace
           values (comma-sep value)
           _ (c/skip-char \])]
    (p/return (into [] values))))

(def object-entry
  (p/with [_ whitespace
           k string
           _ whitespace
           _ (c/skip-char \:)
           v value]
    (p/return #?(:clj  (clojure.lang.MapEntry. k v)
                 :cljs (MapEntry. k v)))))

(def object
  (p/with [_ (c/skip-char \{)
           _ whitespace
           entries (comma-sep object-entry)
           _ (c/skip-char \})]
    (p/return (into {} entries))))

;; TODO: string-return
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

(def json
  (p/with [v value, _ p/eof]
    (p/return v)))

(comment
  (p/parse number "172")
  (p/parse number "172.23e-2")
  (p/parse string "\"foo\\u0044bar\"")
  (p/parse whitespace "   \r\n")

  (p/parse (comma-sep number) "1,2,3,4")

  (p/parse object-entry "\"key\" : 19")
  (p/parse (comma-sep object-entry) "\"key\" : 19, \"bar\": false")

  (p/parse json "  ")
  (p/parse json "[1 2]")
  (p/parse (p/alt (p/attempt number) string) "1.")
  (p/parse json "19")
  (p/parse json "[1, 2, false, null]")
  (p/parse json "{\"foo\": 19, \"bar\" : false, \"x\": [\"y\"]}")

  ;;
  )
