(ns bench.cps
  (:refer-clojure :exclude [char repeat])
  (:require [criterium.core :as criterium]
            [paco.chars :as c]
            [paco.core :as p]
            [paco.error :as error]
            [paco.state :as state]))

(comment
  ;; boolean
  (defn reply [ok? state result error])

  ;; :ok/:error/:fatal
  (defn reply [status state result error])

  ;; false/true/special
  (defn reply [fail? state result error])

  ;; boolean + extra
  (defn reply [ok? state result error ctx])

  ;;
  )

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
        (reply :error state nil (error/merge expected error/unexpected-end))))))

(defn bind [p f]
  (fn [state reply]
    (letfn [(reply1 [status1 state1 value1 error1]
              (if (= :ok status1)
                (let [p2 (f value1)]
                  (if (nil? error1)
                    (p2 state1 reply)
                    (letfn [(reply2 [status2 state2 value2 error2]
                              (if (identical? state2 state1)
                                (reply status2 state2 value2 (error/merge error1 error2))
                                (reply status2 state2 value2 error2)))]
                      (fn []
                        (p2 state1 reply2)))))
                (reply status1 state1 value1 error1)))]
      (fn []
        (p state reply1)))))

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

(defn- state-unchanged-exception [sym p]
  (ex-info (str "Parser supplied to '" sym "' succeeded without changing the parser state")
           {:type ::state-unchanged
            :parser sym
            :arg p}))

(defn- reduce-repeat [sym p rf min max]
  {:pre [(nat-int? min) (or (nil? max) (<= min max))]}
  (letfn [(step [reply acc n state1 error1]
            (letfn [(step-reply [status state2 value error2]
                      (if (= :ok status)
                        (if (identical? state1 state2)
                          (throw (state-unchanged-exception sym p))
                          (let [acc (rf acc value)
                                n   (inc n)]
                            (if (or (nil? max) (< n max))
                              (step reply acc n state2 error2)
                              (reply :ok state2 (rf acc) error2))))
                        (let [error (if (and error1 (identical? state1 state2))
                                      (error/merge error1 error2)
                                      error2)]
                          (if (< n min)
                            (reply :error state2 nil error)
                            (reply :ok state2 (rf acc) error)))))]
              (fn []
                (p state1 step-reply))))]
    (if (and max (pos? max))
      (fn [state reply]
        (step reply (rf) 0 state nil))
      (return (rf (rf))))))

(defn repeat
  ([p n]
   (reduce-repeat `repeat p @#'p/seqexp-rf n n))
  ([p min max]
   (reduce-repeat `repeat p @#'p/seqexp-rf min max)))

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

  (criterium/bench
   (parse (bind (char \f) (fn [x] (return [:char x]))) "foobar"))
  ;;           Execution time mean : 49,057655 ns
  ;;  Execution time std-deviation : 0,494558 ns
  ;; Execution time lower quantile : 48,605355 ns ( 2,5%)
  ;; Execution time upper quantile : 50,150967 ns (97,5%)

  (criterium/bench
   (p/parse (p/bind (c/char \f) (fn [x] (p/return [:char x]))) "foobar"))
  ;;           Execution time mean : 101,514484 ns
  ;;  Execution time std-deviation : 1,189918 ns
  ;; Execution time lower quantile : 100,536222 ns ( 2,5%)
  ;; Execution time upper quantile : 104,472441 ns (97,5%)

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

  ;; repeat

  (parse (repeat (char \x) 0 0) "")
  (parse (repeat (char \x) 0 5) "xxd")

  (run (repeat (char \x) 2 4) "")
  (run (repeat (char \x) 2 4) "x")

  (parse (repeat (char \x) 2 4) "xx")
  (parse (repeat (char \x) 2 4) "xxxx")
  (parse (repeat (char \x) 2 4) "xxxxyy")
  (parse (repeat (char \x) 2 4) "xxxxxx")

  (criterium/bench
   (parse (repeat (char \x) 2 4) "xxxxxx"))
  ;;           Execution time mean : 225,221690 ns
  ;;  Execution time std-deviation : 1,931523 ns
  ;; Execution time lower quantile : 222,721630 ns ( 2,5%)
  ;; Execution time upper quantile : 228,298523 ns (97,5%)

  (criterium/bench
   (p/parse (p/repeat (c/char \x) 2 4) "xxxxxx"))
  ;;           Execution time mean : 302,060060 ns
  ;;  Execution time std-deviation : 2,682887 ns
  ;; Execution time lower quantile : 299,603200 ns ( 2,5%)
  ;; Execution time upper quantile : 303,852685 ns (97,5%)

  ;;
  )
