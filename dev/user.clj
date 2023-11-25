(ns user
  (:require [comparse.core :as p]
            [comparse.char :as c]))

(comment
  (p/run (p/cat (c/char \f)
                (c/char \o)
                (c/char \o))
         "foobar")

  (p/run (c/string "foo")
         "foobar")

  (p/run (c/string "foo")
         "fo")

  (p/run (c/string "foo") "")

  (p/run (p/cat (c/string "foo")
                (c/string "bar"))
         "foobar")

  (p/run (p/cat (c/string "foo")
                (c/string "bar"))
         "foobuzz")

  (p/run (p/attempt (p/cat (c/string "foo")
                           (c/string "bar")))
         "foobuzz")

  (p/run (p/cat (c/string "foo")
                (p/cat (c/string "bu")
                       (c/string "zz")))
         "foobuzz")

  (p/run (p/group (c/string "foo")
                  (p/cat (c/string "bu")
                         (c/string "zz")))
         "foobuzz")

  (p/run (p/satisfy #{\a \b \c} "abc")
         "x")

  (p/run (c/any-of "abc") "x")

  (p/run (p/cat (p/alt (c/string "foo")
                       (p/cat
                        (c/string "bar")
                        (p/satisfy #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0}
                                   "digit")))
                (c/string "buzz"))
         "barbuzz")

  (p/run (-> (c/string "42")
             (p/expected "The answer to life, the universe, and everything."))
         "x")

  (p/run (p/? (c/string "foo"))
         "bar")
  (p/run (p/cat (c/string "foo")
                (p/? (c/string "and"))
                (c/string "bar"))
         "fooandbar")
  (p/run (p/cat (c/string "foo")
                (p/? (c/string "and"))
                (c/string "bar"))
         "foobar")

  (p/run
   (p/rec #(p/alt (c/char \a)
                  (p/cat (c/char \() % (c/char \)))
                  (p/cat (c/char \[) % (c/char \]))
                  (p/cat (c/char \{) % (c/char \}))))
   "([a])")

  (p/run (p/* (c/char \a)) "")
  (p/run (p/* (c/char \a)) "aaaaaab")
  (p/run (p/* (p/? (c/char \a))) "b")

  (p/run (p/+ (c/char \a)) "b")
  (p/run (p/+ (c/char \a)) "aaaaaab")
  (p/run (p/+ (p/? (c/char \a))) "b")

  ;;
  )
