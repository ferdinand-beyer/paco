(ns bench.cps
  (:refer-clojure :exclude [char])
  (:require [criterium.core :as criterium]
            [paco.chars :as c]
            [paco.core :as p]
            [paco.error :as error]
            [paco.state :as state]))

(defn return [x]
  (fn [state reply]
    (reply :ok state x nil)))

(defn char [ch]
  (let [expected (error/expected-input ch)]
    (fn [state reply]
      (if-let [ch2 (state/peek state)]
        (if (= ch ch2)
          (reply :ok (state/skip-char state) ch nil)
          (reply :error state nil (error/merge expected (error/unexpected-input ch2))))
        (reply :error state nil (error/merge expected error/unexpected-eof))))))

(defn bind [p f]
  (fn [state reply]
    (fn []
      (p state
         (fn [status1 state1 value1 error1]
           (if (= :ok status1)
             (let [p2 (f value1)]
               (if (nil? error1)
                 (p2 state1 reply)
                 (fn []
                   (p2 state1
                       (fn [status2 state2 value2 error2]
                         (if (identical? state2 state1)
                           (reply status2 state2 value2 (error/merge error1 error2))
                           (reply status2 state2 value2 error2)))))))
             (reply status1 state1 value1 error1)))))))

(defn pipe
  ([p f]
   (fn [state reply]
     (fn []
       (p state
          (fn [status1 state1 value1 error1]
            (reply status1 state1 (if (= :ok status1) (f value1) value1) error1))))))
  ([p1 p2 f]
   (fn [state reply]
     (fn []
       (p1 state
           (fn [status1 state1 value1 error1]
             (if (= :ok status1)
               (fn []
                 (p2 state1
                     (fn [status2 state2 value2 error2]
                       (reply status2 state2
                              (if (= :ok status2)
                                (f value1 value2)
                                value2)
                              (if (identical? state1 state2)
                                error2
                                (error/merge error1 error2))))))
               (reply status1 state1 value1 error1))))))))

(defn run [p input]
  (let [state (state/of-string input)
        reply (fn [status state value error]
                {:status status, :state state, :value value, :error error})]
    (trampoline #(p state reply))))

(defn parse [p input]
  (let [state (state/of-string input)
        reply (fn [_status _state value _error]
                value)]
    (trampoline #(p state reply))))

(comment
  (run (bind (char \f) #(return [:char %])) "foobar")
  (parse (bind (char \f) #(return [:char %])) "foobar")

  (parse (pipe (char \f) #(str "<"  % ">")) "foobar")

  ;; char

  (criterium/quick-bench
   (parse (char \f) "foobar"))

  (criterium/quick-bench
   (p/parse (c/char \f) "foobar"))

  ;; bind

  (criterium/quick-bench
   (parse (bind (char \f) (fn [x] (return [:char x]))) "foobar"))

  (criterium/quick-bench
   (p/parse (p/bind (c/char \f) #(p/return [:char %])) "foobar"))

  ;; pipe1

  (criterium/quick-bench
   (parse (pipe (char \f) (fn [x] [:char x])) "foobar"))

  (criterium/quick-bench
   (p/parse (p/pipe (c/char \f) (fn [x] [:char x])) "foobar"))

  ;; pipe2

  (criterium/bench
   (parse (pipe (char \f) (char \o)
                (fn [x y] [:pair x y]))
          "foobar"))
  ;;           Execution time mean : 85,277493 ns
  ;;  Execution time std-deviation : 0,921559 ns
  ;; Execution time lower quantile : 84,589873 ns ( 2,5%)
  ;; Execution time upper quantile : 87,465274 ns (97,5%)

  (criterium/bench
   (p/parse (p/pipe (c/char \f) (c/char \o)
                    (fn [x y] [:pair x y]))
            "foobar"))
  ;;           Execution time mean : 133,286075 ns
  ;;  Execution time std-deviation : 1,126201 ns
  ;; Execution time lower quantile : 132,433927 ns ( 2,5%)
  ;; Execution time upper quantile : 133,653385 ns (97,5%)

  ;;
  )
