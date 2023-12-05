(ns bench.direct
  (:refer-clojure :exclude [char])
  (:require [criterium.core :as criterium]
            [paco.chars :as c]
            [paco.core :as p]
            [paco.error :as error]
            [paco.state :as state]))

(defrecord Reply [status state value error])

(defn call [p state]
  (trampoline #(p state)))

(defmacro with-reply [binding & body]
  (let [[sym [p state]] binding]
    `(fn []
       (let [~(with-meta sym {:tag `Reply}) (call ~p ~state)]
         ~@body))))

(defmacro ok? [reply]
  `(= :ok (.-status ~(with-meta reply {:tag `Reply}))))

(defmacro no-error? [reply]
  `(nil? (.-error ~(with-meta reply {:tag `Reply}))))

(defmacro state-unchanged? [reply1 reply2]
  `(identical? (.-state ~(with-meta reply1 {:tag `Reply}))
               (.-state ~(with-meta reply2 {:tag `Reply}))))

(defmacro with-error [reply error]
  (let [reply (with-meta reply {:tag `Reply})]
    `(Reply. (.-status ~reply)
             (.-state ~reply)
             (.-value ~reply)
             ~error)))

(defn return [x]
  (fn [state]
    (Reply. :ok state x nil)))

(defn char [ch]
  (let [expected (error/expected-input ch)]
    (fn [state]
      (if-let [ch2 (state/peek state)]
        (if (= ch ch2)
          (Reply. :ok (state/skip-char state) ch nil)
          (Reply. :error state nil (error/merge expected (error/unexpected-input ch2))))
        (Reply. :error state nil (error/merge expected error/unexpected-end))))))

(defn bind [p f]
  (fn [state]
    (with-reply [reply1 (p state)]
      (if (ok? reply1)
        (let [p2 (f (.-value reply1))]
          (if-let [error (.-error reply1)]
            (with-reply [reply2 (p2 (.-state reply1))]
              (if (state-unchanged? reply1 reply2)
                (with-error reply2 (error/merge error (.-error reply2)))
                reply2))
            (p2 (.-state reply1))))
        reply1))))

(comment
  (defn bind [p f]
    (fn [state]
      (fn []
        (let [^Reply reply1 (call p state)]
          (if (= :ok (.-status reply1))
            (let [p2 (f (.-value reply1))]
              (if (nil? (.-error reply1))
                (p2 (.-state reply1))
                (let [^Reply reply2 (call p2 (.-state reply1))]
                  (if (identical? (.-state reply1) (.-state reply2))
                    (Reply. (.-status reply2)
                            (.-state reply2)
                            (.-value reply2)
                            (error/merge (.-error reply1) (.-error reply2)))
                    reply2))))
            reply1))))))

(defn run [p input]
  (call p (state/of-string input)))

(defn parse [p input]
  (let [^Reply reply (run p input)]
    (if (= :ok (.-status reply))
      (.-value reply)
      (error/string (.-error reply)))))

(comment
  (run (bind (char \f) #(return [:char %])) "foobar")
  (parse (bind (char \f) #(return [:char %])) "foobar")

  (criterium/quick-bench
   (parse (char \f) "foobar"))

  (criterium/quick-bench
   (p/parse (c/char \f) "foobar"))

  (criterium/quick-bench (bind (char \f) #(return [:char %])))
  (criterium/quick-bench (p/bind (c/char \f) #(p/return [:char %])))

  (criterium/quick-bench
   (parse (bind (char \f) #(return [:char %])) "foobar"))

  (criterium/quick-bench
   (p/parse (p/bind (c/char \f) #(p/return [:char %])) "foobar"))

  ;;
  )
