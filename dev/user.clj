(ns user
  (:require [comparse.core :as c]))

(comment
  (c/run (c/cat (c/char \f)
                (c/char \o)
                (c/char \o))
         "foobar")

  (c/run (c/string "foo")
         "foobar")

  (c/run (c/string "foo")
         "fo")

  (c/run (c/string "foo") "")

  (c/run (c/cat (c/string "foo")
                (c/string "bar"))
         "foobar")

  (c/run (c/cat (c/string "foo")
                (c/string "bar"))
         "foobuzz")

  (c/run (c/attempt (c/cat (c/string "foo")
                           (c/string "bar")))
         "foobuzz")

  (c/run (c/cat (c/string "foo")
                (c/cat (c/string "bu")
                       (c/string "zz")))
         "foobuzz")

  (c/run (c/pseq (c/string "foo")
                 (c/cat (c/string "bu")
                        (c/string "zz")))
         "foobuzz")

  (c/run (c/satisfy #{\a \b \c} "abc")
         "x")

  (c/run (c/cat (c/alt (c/string "foo")
                       (c/cat
                        (c/string "bar")
                        (c/satisfy #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0}
                                   "digit")))
                (c/string "buzz"))
         "barbuzz")

  (c/run (-> (c/string "42")
             (c/expected "The answer to life, the universe, and everything."))
         "x")

  (c/run (c/? (c/string "foo"))
         "bar")
  (c/run (c/cat (c/string "foo")
                (c/? (c/string "and"))
                (c/string "bar"))
         "fooandbar")

  (c/run
   (c/rec #(c/alt (c/char \a)
                  (c/cat (c/char \() % (c/char \)))
                  (c/cat (c/char \[) % (c/char \]))
                  (c/cat (c/char \{) % (c/char \}))))
   "([a])")

  ;;
  )
