(ns comparse.char
  (:refer-clojure :exclude [cat char])
  (:require [comparse.base :as base]
            [comparse.error :as error]
            [comparse.reply :as reply]
            [comparse.state :as state]))

(defn char [c]
  (base/match #(= c %) c))

(defn string [s]
  {:pre [(not (re-find #"[\r\n]" s))]}
  (let [length (count s)
        errors #(list % (error/expected-str s))]
    (fn [state]
      (if (state/matches? state s)
        (reply/success (state/skip* state length) s)
        (reply/failure state
                       (errors
                        (if (state/at-end? state)
                          error/unexpected-eof
                          (error/unexpected (state/peek-char state)))))))))

(comment
  (base/run (base/cat (char \f)
                      (char \o)
                      (char \x))
            "foobar")

  (base/run (string "foo")
            "foobar")

  ;;

  )
