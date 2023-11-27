(ns user
  (:require [comparse.chars :as c]
            [comparse.core :as p]
            [criterium.core :as criterium]))

(comment
  (p/parse (p/pipe (c/char \1)
                   (c/char \2)
                   (c/char \3)
                   (c/char \4)
                   (c/char \5)
                   (fn [& args]
                     (zipmap args [:a :b :c :d :e :f :g])))
           "12345678")

  (p/parse (p/>> (c/string "Just")
                 (c/char \space)
                 (c/string "do"))
           "Just do")

  (p/parse (p/cat (c/char \f)
                  (c/char \o)
                  (c/char \o))
           "foobar")

  (p/parse (c/string "foo")
           "foobar")

  (p/parse (c/string "foo")
           "fo")

  (p/parse (c/string "foo") "")

  (p/parse (p/cat (c/string "foo")
                  (c/string "bar"))
           "foobar")

  (p/parse (p/cat (c/string "foo")
                  (c/string "bar"))
           "foobuzz")

  (p/parse (p/attempt (p/cat (c/string "foo")
                             (c/string "bar")))
           "foobuzz")

  (p/parse (p/cat (c/string "foo")
                  (p/cat (c/string "bu")
                         (c/string "zz")))
           "foobuzz")

  (p/parse (p/group (c/string "foo")
                    (p/cat (c/string "bu")
                           (c/string "zz")))
           "foobuzz")

  (p/parse (c/match #{\a \b \c} "abc")
           "x")

  (p/parse (c/any-of "abc") "x")

  (p/parse (p/cat (p/alt (c/string "foo")
                         (p/cat
                          (c/string "bar")
                          (c/match #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0}
                            "digit")))
                  (c/string "buzz"))
           "barbuzz")

  (p/parse (-> (c/string "42")
               (p/expected "The answer to life, the universe, and everything."))
           "x")

  (p/parse (p/? (c/string "foo"))
           "bar")
  (p/parse (p/cat (c/string "foo")
                  (p/? (c/string "and"))
                  (c/string "bar"))
           "fooandbar")
  (p/parse (p/cat (c/string "foo")
                  (p/? (c/string "and"))
                  (c/string "bar"))
           "foobar")

  (p/parse
   (p/rec #(p/alt (c/char \a)
                  (p/cat (c/char \() % (c/char \)))
                  (p/cat (c/char \[) % (c/char \]))
                  (p/cat (c/char \{) % (c/char \}))))
   "([a])")

  (p/parse (p/* (c/char \a)) "")
  (p/parse (p/* (c/char \a)) "aaaaaab")
  (p/parse (p/* (p/? (c/char \a))) "b")

  (p/parse (p/+ (c/char \a)) "b")
  (p/parse (p/+ (c/char \a)) "aaaaaab")
  (p/parse (p/+ (p/? (c/char \a))) "b")

  (let [p (p/cat (c/string "the")
                 (c/char \space)
                 (c/string "quick")
                 (c/char \space)
                 (c/string "brown")
                 (c/char \space)
                 (c/string "fox")
                 (c/char \space)
                 (c/string "jumps")
                 (c/char \space)
                 (c/string "over")
                 (c/char \space)
                 (c/string "the")
                 (c/char \space)
                 (c/string "lazy")
                 (c/char \space)
                 (c/string "dog"))]
    (criterium/quick-bench
     (p/parse p "the quick brown fox jumps over the lazy dog")))
  ;; cons: Execution time mean : 1,078848 µs
  ;; reduce: Execution time mean : 876,197052 ns

  (p/parse (p/cat (p/? (p/cat (c/string "foo")
                              (c/string "bar")))
                  (c/string "foox"))
           "foox")

  (p/parse (p/cat (p/?? (p/cat (c/string "foo")
                               (c/string "bar")))
                  (c/string "foox"))
           "foox")

  (p/parse (p/cat (p/? (p/attempt (p/cat (c/string "foo")
                                         (c/string "bar"))))
                  (c/string "foox"))
           "foox")

  (let [p (p/cat (p/?? (p/cat (c/string "foo")
                              (c/string "bar")))
                 (c/string "foox"))]
    (criterium/quick-bench
     (p/parse p "foox")))

  (let [p (p/cat (p/? (p/attempt (p/cat (c/string "foo")
                                        (c/string "bar"))))
                 (c/string "foox"))]
    (criterium/quick-bench
     (p/parse p "foox")))

  (p/parse (-> (c/char \a)
               (p/skip*)
               (p/cat (c/char \b))
               (p/pipe first))
           "aaaaaaaaaab")

;;
  )
