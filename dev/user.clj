(ns user
  (:require [comparse.char :as c]
            [comparse.core :as p]
            [criterium.core :as criterium]))

(comment
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

  (p/parse (p/satisfy #{\a \b \c} "abc")
           "x")

  (p/parse (c/any-of "abc") "x")

  (p/parse (p/cat (p/alt (c/string "foo")
                         (p/cat
                          (c/string "bar")
                          (p/satisfy #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0}
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

  (p/run (p/pipe (c/char \a)
                 (c/char \b)
                 (c/char \c)
                 (c/char \d)
                 (fn [a b c d]
                   {:a a, :b b, :c c, :d d}))
         "abcd")

  (def ps [(c/char \a)
           (c/char \b)
           (c/char \c)
           (c/char \d)
           (c/char \e)
           (c/char \f)])

  (def p1 (p/series ps))
  (def p2 (p/series2 ps))

  (criterium/quick-bench
   (p/run p1 ""))
  (criterium/quick-bench
   (p/run p2 ""))

  (criterium/quick-bench
   (p/run p1 "abc"))
  (criterium/quick-bench
   (p/run p2 "abc"))

  (criterium/quick-bench
   (p/run p1 "abcdex"))
  (criterium/quick-bench
   (p/run p2 "abcdex"))

;;
  )
