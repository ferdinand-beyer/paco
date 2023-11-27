(ns examples.json
  (:require [paco.core :as p]
            [paco.chars :as c]
            [clojure.string :as str]))

;; TODO: skip-match
;; TODO: <??>, label fail! (aka, as)
(def whitespace
  (p/* (c/any-of " \r\n\t")))

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

;; TODO: c/char-return
;; TODO: (p/between p popen pclose)
(def string
  (p/with [_ (c/char \")
           s (p/* (p/alt (p/>> (c/char \\)
                               (p/alt (c/any-of "\"\\/")
                                      (p/>> (c/char \b) (p/return \backspace))
                                      (p/>> (c/char \f) (p/return \formfeed))
                                      (p/>> (c/char \n) (p/return \newline))
                                      (p/>> (c/char \r) (p/return \return))
                                      (p/>> (c/char \t) (p/return \tab))
                                      (p/>> (c/char \u)
                                            (p/pipe
                                             (p/repeat c/hex 4)
                                             (fn [chs]
                                               (-> (str/join chs)
                                                   (#?(:clj Long/parseLong
                                                       :cljs js/parseInt)  16)
                                                   char))))))
                         (c/match #(not (or (#{\" \\} %)
                                            (c/control? %))))))
           _ (c/char \")]
    (p/return (str/join s))))

(declare value)

;; TODO: p/sep-by
(defn comma-sep [p]
  (p/? (p/with [x p
                xs (p/* (p/>> (c/char \,) p))]
         (p/return (cons x xs)))))

(def array
  (p/with [_ (c/char \[)
           _ whitespace
           values (comma-sep value)
           _ (c/char \])]
    (p/return (into [] values))))

(def object-entry
  (p/with [_ whitespace
           k string
           _ whitespace
           _ (c/char \:)
           v value]
    (p/return #?(:clj  (clojure.lang.MapEntry. k v)
                 :cljs (MapEntry. k v)))))

(def object
  (p/with [_ (c/char \{)
           _ whitespace
           entries (comma-sep object-entry)
           _ (c/char \})]
    (p/return (into {} entries))))

;; TODO: string-return
(def value
  (p/with [_ whitespace
           v (p/alts [string
                      number
                      object
                      array
                      (p/>> (c/string "true") (p/return true))
                      (p/>> (c/string "false") (p/return false))
                      (p/>> (c/string "null") p/pnil)]
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
  (p/parse json "19")
  (p/parse json "[1, 2, false, null]")
  (p/parse json "{\"foo\": 19, \"bar\" : false, \"x\": \"y\"}")

  ;;
  )
