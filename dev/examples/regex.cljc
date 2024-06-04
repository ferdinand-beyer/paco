(ns examples.regex
  (:require [clojure.string :as str]
            [paco.char :as c]
            [paco.core :as p]))

(def posix-char-class
  (-> ["Lower"
       "Upper"
       "ASCII"
       "Alpha"
       "Digit"
       "Alnum"
       "Punct"
       "Graph"
       "Print"
       "Blank"
       "Cntrl"
       "XDigit"
       "Space"]
      (->> (map #(c/string-return % (keyword "posix" (str/lower-case %)))))
      (p/alts "POSIX character class")))

(def unicode-binary-property
  (-> ["Alphabetic"
       "Ideographic"
       "Letter"
       "Lowercase"
       "Uppercase"
       "Titlecase"
       "Punctuation"
       "Control"
       "White_Space"
       "Digit"
       "Hex_Digit"
       "Join_Control"
       "Noncharacter_Code_Point"
       "Assigned"
       "Emoji"
       "Emoji_Presentation"
       "Emoji_Modifier"
       "Emoji_Modifier_Base"
       "Emoji_Component"
       "Extended_Pictographic"]
      (->> (map #(c/string-return % (keyword "unicode" (-> % (str/replace \_ \-) str/lower-case)))))
      (p/alts "Unicode binary property")))

(def unicode-name
  (c/regex #"[A-Z][a-zA-Z0-9_]*" "unicode name"))

(def unicode-script
  (-> (p/alt (c/string "script=")
             (c/string "sc="))
      (p/then unicode-name)
      (p/map #(vector :unicode/script %))))

(def unicode-block
  (-> (p/alt (c/string "In")
             (c/string "block=")
             (c/string "blk="))
      (p/then unicode-name)
      (p/map #(vector :unicode/block %))))

(def unicode-category
  (-> (p/alt (c/string "general_category=")
             (c/string "gc="))
      (p/then unicode-name)
      (p/map #(vector :unicode/category %))))

(def unicode-property
  (p/alt unicode-block
         unicode-script
         unicode-category
         (-> (c/string "Is")
             (p/then (p/alt unicode-binary-property
                            (p/map unicode-name #(vector :unicode/script-or-category %)))))
         (p/map unicode-name #(vector :unicode/category %))))

(def char-property
  (-> (p/alt posix-char-class unicode-property)
      (p/between (c/char \{) (c/char \}))))

(def group-name
  (-> (c/regex #"[a-zA-Z][a-zA-Z0-9_]*" "group name")
      (p/between (c/char \<) (c/char \>))))

(def escape-sequence
  (-> (c/char \\)
      (p/then (p/alt (c/char \\)
                     (-> (c/char \0)
                         (p/then (-> c/octal
                                     ;; TODO: Only 1,2; with 3 chars the first one must be in 0..3
                                     (p/repeat 1 3)
                                     (p/map #(into [:octal] %)))))
                     (-> (c/char \x)
                         ;; TODO: \x{h...h}
                         (p/then (-> c/hex
                                     (p/repeat 2)
                                     (p/map #(into [:hex] %)))))
                     (-> (c/char \u)
                         (p/then (-> c/hex
                                     (p/repeat 4)
                                     (p/map #(into [:hex] %)))))
                     (c/char-return \t :tab)
                     (c/char-return \n :newline)
                     (c/char-return \r :return)
                     (c/char-return \f :formfeed)
                     (c/char-return \a :alert)
                     (c/char-return \e :escape)
                     ;; TODO \cx -- control char corresponding to x

                     ;; predefined character classes
                     (c/char-return \d :digit)
                     (c/char-return \D :non-digit)
                     (c/char-return \h :horizontal)
                     (c/char-return \H :non-horizontal)
                     (c/char-return \s :whitespace)
                     (c/char-return \S :non-whitespace)
                     (c/char-return \v :vertical)
                     (c/char-return \V :non-vertical)
                     (c/char-return \w :word)
                     (c/char-return \W :non-word)

                     ;; character properties
                     (-> (c/char \p)
                         (p/then char-property))
                     (-> (c/char \P)
                         (p/then char-property)
                         (p/map #(vector :not-class %)))

                     ;; boundary matchers
                     (c/char-return \b :boundary) ;; TODO: \b{g}
                     (c/char-return \B :non-boundary)
                     (c/char-return \A :input-start)
                     (c/char-return \G :prev-match)
                     (c/char-return \Z :input-end*)
                     (c/char-return \z :input-end)

                     (c/char-return \R :linebreak)

                     (c/char-return \X :grapheme-cluster) ;; Any Unicode extended grapheme cluster

                     ;; back references
                     (p/map c/digit #(vector :ref %))
                     (-> (c/char \k)
                         (p/then group-name)
                         (p/map #(vector :ref %)))

                     ;; quotation
                     (-> (c/char \Q)
                         (p/then (c/strcat (p/*until c/any-char (p/alt (c/string "\\E") p/end)))))))))

;; TODO:
;; [abc]
;; [^abc]
;; [a-zA-Z]
;; [a-d[m-p]]
;; [a-z&&[def]] -- d,e or f (intersection)
;; [a-z&&[^bc]] -- subtraction
;; [a-z&&[^m-p]] -- subtraction
;; http://www.unicode.org/reports/tr18/
(def char-class
  (-> (c/none-of "]")
      (p/*)
      (p/between (c/char \[) (c/char \]))
      (p/map #(into [:class] %))))

(declare pattern)

;; TODO: SPECIAL CONSTRUCTS (NAMED-CAPTURING AND NON-CAPTURING)
;; (?<name>X)
;; (?:X) -- non-capturing group
;; (?idmsuxU-idmsuxU) -- turn flags on/off
;; (?idmsuxU-idmsuxU:X) -- X as a non-capturing group with flags on/off
;; (?=X) -- X via positive lookahead
;; (?!X) -- X via negative lookahead
;; (?<=X) -- X via positive lookbehind
;; (?<!X) -- X via negative lookbehind
;; (?>X) -- X as an independent non-capturing group
(def group
  (-> (p/fwd pattern)
      (p/between (c/char \() (c/char \)))
      (p/map #(conj [:group] %))))

(def ordinary-char (c/none-of ".*+?^${}()|[]\\"))

(def regex-atom
  (p/alt ordinary-char
         escape-sequence
         (c/char-return \. :any)
         (c/char-return \^ :start)
         (c/char-return \$ :end)
         group
         char-class))

(def decimal
  (p/map (c/strcat (p/+ c/digit)) parse-long))

(def ordinary-quantifier
  (p/alt (c/char-return \? [:? {}])
         (c/char-return \* [:* {}])
         (c/char-return \+ [:+ {}])
         (-> (p/map decimal (p/? (p/cat (c/char \,) (p/? decimal)))
                    (fn [min [range? max]]
                      [:repeat {:min min, :max (if range? max min)}]))
             (p/between (c/char \{) (c/char \})))))

(def quantifier
  (p/map ordinary-quantifier
         (p/alt (c/char-return \? :reluctant)
                (c/char-return \+ :possessive)
                (p/return :greedy))
         (fn [q kind]
           (assoc-in q [1 :kind] kind))))

(def piece
  (p/map regex-atom (p/? quantifier)
         (fn [a q]
           (if q
             (conj q a)
             a))))

(def branch
  (-> (p/* piece)
      (p/map (fn [pieces]
               (if-some [fst (first pieces)]
                 (if (next pieces)
                   (into [:cat] pieces)
                   fst)
                 [:cat])))))

(def pattern
  (-> branch
      (p/+sep-by (c/char \|))
      (p/map (fn [branches]
               (if (next branches)
                 (into [:alt] branches)
                 (first branches))))))

(def regex (p/then-skip pattern p/end))

(comment
  (p/parse regex "xxxx??|x|x")

  (p/parse regex "xxxx??|(x)|x\\q")

  (p/parse regex "(hello)*\\01")

;;
  )
