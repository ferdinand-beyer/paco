(ns examples.json
  (:refer-clojure :exclude [array])
  (:require [paco.char :as c]
            [paco.char.preds :as preds]
            [paco.core :as p]
            [paco.detail.error :as error]
            [paco.detail.parser :as parser]
            [paco.detail.reply :as reply]
            [paco.detail.source :as source])
  #?(:clj (:import [clojure.lang MapEntry])))

(def skip-whitespace
  (c/*skip-satisfy (preds/among " \r\n\t")))

(defn number [source reply]
  (if-let [m (source/re-match source #"-?(?:0|[1-9][0-9]*)(\.[0-9]+)?([eE][-+]?[0-9]+)?")]
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
              #?(:clj (Double/valueOf s), :cljs (js/parseFloat s)))]
      (source/skip! source n)
      (reply/ok reply v))
    (reply/fail reply (error/merge (error/unexpected-token-or-end source)
                                   (error/expected "number")))))

(defn- parse-int [s radix]
  #?(:clj  (Long/parseLong s radix)
     :cljs (js/parseInt s radix)))

(def slow-string-remainder
  (let [unicode  (-> (p/repeat c/hex 4)
                     c/skipped
                     (p/map #(char (parse-int % 16))))
        dispatch (p/alt (c/any-of "\"\\/")
                        (c/char-return \b \backspace)
                        (c/char-return \f \formfeed)
                        (c/char-return \n \newline)
                        (c/char-return \r \return)
                        (c/char-return \t \tab)
                        (p/then (c/skip-char \u) unicode))
        escape   (p/then (c/skip-char \\) dispatch)
        regular  (-> (preds/not (preds/or (preds/among "\"\\") preds/control?))
                     (c/satisfy "regular character"))
        char     (p/alt escape regular)
        quote    (c/skip-char \")]
    (fn [prefix]
      (-> (c/strcat (p/return prefix) dispatch (p/* char))
          (p/then-skip quote)))))

(def string
  (let [quote    (preds/eq \")
        regular  (preds/not-among "\"\\")
        expected (error/expected-input \")
        pstring  (fn [source reply]
                   ;; skip over start quote
                   (source/skip! source)
                   (source/with-resource [mark (source/mark source)]
                     (loop []
                       (if (source/read-char-when! source regular)
                         (recur)
                         ;; found closing quote, escape sequence, or end
                         (if-let [ch (source/peek source)]
                           (let [s (source/read-from source mark)]
                             (source/skip! source)
                             (if (= \" ch)
                               (reply/ok reply s)
                               (let [p (slow-string-remainder s)]
                                 (parser/apply p source reply))))
                           (reply/fail reply (error/merge error/unexpected-end expected)))))))]
    (fn [source reply]
      (if (source/satisfies-char-pred? source quote)
        (pstring source reply)
        (reply/fail reply expected)))))

(defn comma-sep [p]
  (p/*sep-by p (c/skip-char \,)))

(declare value)

(def array
  (-> skip-whitespace
      (p/then (comma-sep (p/fwd value)))
      (p/between (c/skip-char \[) (c/skip-char \]))))

(comment
  (p/parse array "[1, 2, 3, 4]")
  ;;
  )

(def object-entry
  (p/map (p/then skip-whitespace string)
         (p/then skip-whitespace (c/skip-char \:) (p/fwd value))
         #(MapEntry. %1 %2)))

(def object
  (-> skip-whitespace
      (p/then (comma-sep object-entry))
      (p/map #(into {} %))
      (p/between (c/char \{) (c/char \}))))

(def value
  (-> (p/alts [string
               number
               object
               array
               (c/string-return "true" true)
               (c/string-return "false" false)
               (c/string-return "null" nil)]
              "value")
      (p/between skip-whitespace)))

(def json (p/then-skip value p/end))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment
  (p/parse number "172")
  (p/parse number "172.23e-2")
  (p/parse string "\"foo\\u0044bar\"")
  (p/parse skip-whitespace "   \r\n")

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
  (p/parse (p/alt (p/atomic number) string) "1.")
  (p/parse json "19")
  (p/parse json "[1, 2, false, null]")
  (p/parse json "{\"foo\": 19, \"bar\" : false, \"x\": [\"y\"]}")

  (require '[clojure.data.json :as json])

  (def input (slurp "dev/examples/json/example4.json"))
  (def input (slurp "dev/examples/json/citm_catalog.json"))

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
  ;; jvm: Execution time mean : 116,270402 ms
  ;; inline: Execution time mean : 86,652540 ms
  ;; char-preds: Execution time mean : 74,400620 ms
  ;; skip-whitespace: Execution time mean : 50,419218 ms
  ;; faster string: Execution time mean : 31,209764 ms
  ;; avoid bind: Execution time mean : 26,639856 ms

  ;; v0.1
  ;; Execution time mean : 202,187895 ms

  (criterium/quick-bench
   (p/parse json input {:line-tracking? false}))
  ;; Execution time mean : 135,796054 ms
  ;; faster string: Execution time mean : 29,227533 ms
  ;; avoid bind: Execution time mean : 23,530859 ms

  ;;
  )
