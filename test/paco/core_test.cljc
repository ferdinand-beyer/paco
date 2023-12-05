(ns paco.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [paco.chars :as c]
            [paco.core :as p]
            [paco.detail :as detail]
            [paco.error :as error]
            [paco.helper :as helper]
            [paco.pos :as pos])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(deftest pnil-test
  (let [reply (helper/run p/pnil)]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (nil? (:error reply)))))

(deftest fail-test
  (let [reply (helper/run (p/fail "boom!"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (= (error/message "boom!") (:error reply)))))

(deftest return-test
  (let [reply (helper/run (p/return 1))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 1 (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/return (p/return 1) 2))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 2 (:value reply))))

  (let [reply (helper/run (p/return helper/any 2) "test")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= 2 (:value reply))))

  (let [reply (helper/run (p/return (p/fail "boom!") 2))]
    (is (:fail? reply))
    (is (not (:changed? reply)))))

(deftest eof-test
  (let [reply (helper/run p/eof)]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run p/eof "test")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (= error/expected-eof (:error reply)))))

(deftest bind-test
  (let [reply (helper/run (p/bind (p/return 1) #(p/return [% 2])))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= [1 2] (:value reply))))

  (let [reply (helper/run (p/bind helper/any #(p/return [% 2])) "test")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\t 2] (:value reply))))

  (let [reply (helper/run (p/bind p/pnil (constantly helper/any)) "test")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \t (:value reply)))))

(deftest with-test
  (let [p (p/with [a helper/any, b helper/any, c helper/any]
            (p/return (zipmap [:a :b :c] [a b c])))]
    (is (= {:a \a, :b \b, :c \c} (p/parse p "abcdef")))))

(deftest pipe-test
  (let [reply (helper/run (p/pipe (p/return 1) inc))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 2 (:value reply))))

  (let [reply (helper/run (p/pipe (p/return 1) (p/return 2) +))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 3 (:value reply))))

  (let [reply (helper/run (p/pipe (p/return 1) (p/return 2) (p/return 3) +))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 6 (:value reply))))

  (let [reply (helper/run (p/pipe (p/return 1) (p/return 2) (p/return 3)
                                  (p/return 4) +))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 10 (:value reply))))

  (let [reply (helper/run (p/pipe (p/return 1) (p/return 2) (p/return 3)
                                  (p/return 4) (p/return 5) +))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 15 (:value reply))))

  (let [reply (helper/run (p/pipe helper/any helper/any helper/any vector) "abc")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\a \b \c] (:value reply))))

  (let [reply (helper/run (p/pipe helper/any helper/any helper/any
                                  helper/any helper/any helper/any
                                  vector) "abcdef")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\a \b \c \d \e \f] (:value reply))))

  (let [reply (helper/run (p/pipe helper/any helper/any helper/any
                                  helper/any helper/any helper/any
                                  (p/fail "boom!") vector) "abcdef")]
    (is (:fail? reply))
    (is (:changed? reply))
    (is (nil? (:value reply)))
    (is (= (error/message "boom!") (:error reply)))))

(deftest sequence-test
  (let [reply (helper/run (p/sequence [(p/return :begin) (p/return :end)]))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= [:begin :end] (:value reply))))

  (let [reply (helper/run (p/sequence [(p/return :begin) helper/any (p/return :end)]) "test")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [:begin \t :end] (:value reply))))

  (let [reply (helper/run (p/sequence [(p/return :begin) (p/fail "boom!")]))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/message "boom!") (:error reply))))

  (let [reply (helper/run (p/sequence [helper/any (p/fail "boom!")]) "test")]
    (is (:fail? reply))
    (is (:changed? reply))
    (is (= (error/message "boom!") (:error reply)))))

(deftest group-test
  (is (= [\a \b \c] (p/parse (p/group helper/any helper/any helper/any) "abcdef"))))

(deftest then-test
  (is (= \a (p/parse (p/then helper/any) "abcdef")))
  (is (= \c (p/parse (p/then helper/any helper/any helper/any) "abcdef"))))

(deftest then-skip-test
  (is (= \a (p/parse (p/then-skip helper/any) "abcdef")))
  (is (= \a (p/parse (p/then-skip helper/any helper/any helper/any) "abcdef"))))

(deftest between-test
  (is (= \x (p/parse (-> helper/any (p/between (c/char \() (c/char \)))) "(x)"))))

(deftest attempt-test
  (is (= :foo-fighters (-> (p/alt (p/attempt (p/cat (c/string "foo")
                                                    (c/string "bar")))
                                  (p/return (c/string-i "foo fighters") :foo-fighters))
                           (p/parse "Foo Fighters are a band"))))

  (let [reply (helper/run (p/attempt helper/any))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/attempt (p/then helper/any helper/any)) "x")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= ::error/nested (get-in reply [:error :type])))
    (is (= ::error/unexpected (get-in reply [:error :error :type])))))

(deftest ?!-test
  (let [reply (helper/run (p/?! helper/any))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/?! helper/any ::default))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= ::default (:value reply))))

  (let [reply (helper/run (p/?! (p/then helper/any helper/any)) "x")]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= ::error/nested (get-in reply [:error :type]))))

  (let [reply (helper/run (p/?! (p/then helper/any helper/any) ::default) "x")]
    (is (:ok? reply))
    (is (= ::default (:value reply)))))

(deftest alt-test
  (is (= \b (p/parse (p/alt (c/char \a) (c/char \b)) "bingo")))

  (let [reply (helper/run (p/alt (p/return 1) helper/any))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 1 (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/alt helper/any (p/return 1)))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= 1 (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/alt (c/char \x) helper/any) "test")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \t (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/alt (p/fail "boom!") (p/fail "bang!")))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= #{(error/message "boom!")
             (error/message "bang!")}
           (:messages reply)))))

(deftest alts-test
  (let [reply (helper/run (p/alts [(p/fail "boom!") helper/any]) "test")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \t (:value reply)))
    (is (nil? (:error reply))))

  (testing "reports 'expected' error with given label"
    (let [reply (helper/run (p/alts [(p/fail "boom!") (p/fail "bang!")] "failure"))]
      (is (:fail? reply))
      (is (not (:changed? reply)))
      (is (= (error/expected "failure") (:error reply))))))

(deftest not-empty-test
  (is (= \t (p/parse (p/not-empty helper/any) "test")))
  (let [reply (helper/run (p/not-empty (p/return ::empty)))]
    (is (:fail? reply))
    (is (nil? (:error reply)))))

(deftest followed-by-test
  (let [reply (helper/run (p/followed-by helper/any) "abc")]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/followed-by (c/char \x)) "abc")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/followed-by helper/any))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/followed-by (c/char \x) "'x' comes next") "abc")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected "'x' comes next") (:error reply)))))

(deftest not-followed-by-test
  (let [reply (helper/run (p/not-followed-by helper/any) "abc")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/not-followed-by (c/char \x)) "abc")]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/not-followed-by helper/any))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/not-followed-by (c/char \x) "'x' comes next") "xyz")]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/unexpected "'x' comes next") (:error reply)))))

(deftest look-ahead-test
  (is (= \a (p/parse (p/look-ahead helper/any) "abc")))

  (let [reply (helper/run (p/look-ahead helper/any))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= ::error/nested (get-in reply [:error :type])))
    (is (= error/unexpected-eof (get-in reply [:error :error]))))

  (let [reply (helper/run (p/look-ahead (p/fatal "test")))]
    (is (:fail? reply))
    (is (detail/error? (:status reply)))
    (is (not (:changed? reply)))
    (is (= ::error/nested (get-in reply [:error :type])))
    (is (= (error/message "test") (get-in reply [:error :error])))))

(deftest as-test
  (let [reply (helper/run (p/as helper/any "something"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected "something") (:error reply))))

  (let [reply (helper/run (p/as helper/any "something") "test")]
    (is (:ok? reply))
    (is (= \t (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/as (p/then helper/any helper/any) "something") "x")]
    (is (:fail? reply))
    (is (:changed? reply))
    (is (= error/unexpected-eof (:error reply)))))

(deftest as!-test
  (let [reply (helper/run (p/as! helper/any "something"))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= (error/expected "something") (:error reply))))

  (let [reply (helper/run (p/as! helper/any "something") "test")]
    (is (:ok? reply))
    (is (= \t (:value reply)))
    (is (nil? (:error reply))))

  (testing "reports compound error"
    (let [reply (helper/run (p/as! (p/then helper/any helper/any) "something") "x")]
      (is (detail/fatal? (:status reply)))
      (is (not (:changed? reply)))
      (is (= ::error/compound (get-in reply [:error :type])))
      (is (= "something" (get-in reply [:error :label]))))

    (let [reply (helper/run (-> (p/then helper/any helper/any)
                                (p/as! "two chars")
                                p/attempt)
                            "x")]
      (is (= ::detail/error (:status reply)))
      (is (= ::error/compound (get-in reply [:error :type])))
      (is (= ::error/unexpected (get-in reply [:error :error :type]))))))

(deftest cat-test
  (is (= [\a \b \c \d] (p/parse (p/cat (p/cat helper/any helper/any)
                                       (p/cat helper/any helper/any))
                                "abcd"))))

(deftest cats-test
  (is (= [\a \b \c] (p/parse (p/cats [helper/any helper/any helper/any])
                             "abcd"))))

(deftest ?-test
  (let [reply (helper/run (p/? helper/any) "x")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= \x (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/? helper/any))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (nil? (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/? helper/any ::default))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= ::default (:value reply)))
    (is (= error/unexpected-eof (:error reply)))))

(deftest *-test
  (is (empty? (p/parse (p/* (c/char \a)) "bbb")))
  (is (= [\a \a \a] (p/parse (p/* (c/char \a)) "aaabbb")))

  (is (thrown? ExceptionInfo (helper/run (p/* p/pnil)))
      "must not accept empty input")

  (let [reply (helper/run (p/* helper/any))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (empty? (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/* (c/char \a)) "ab")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\a] (:value reply)))
    (is (= #{(error/expected-input \a)
             (error/unexpected-input \b)}
           (:messages reply)))))

(deftest +-test
  (let [reply (helper/run (p/+ helper/any))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-eof (:error reply)))))

(deftest skip*-test
  (let [reply (helper/run (p/skip* helper/any))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/skip* helper/any) "xxxx")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (nil? (:value reply)))
    (is (= 0 (-> reply :state pos/line-index)))
    (is (= 4 (-> reply :state pos/column-index)))
    (is (= error/unexpected-eof (:error reply)))))

(deftest skip+-test
  (let [reply (helper/run (p/skip+ helper/any))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/skip+ helper/any) "xxxx")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (nil? (:value reply)))
    (is (= 0 (-> reply :state pos/line-index)))
    (is (= 4 (-> reply :state pos/column-index)))
    (is (= error/unexpected-eof (:error reply)))))

(deftest min-test
  (let [reply (helper/run (p/min helper/any 2))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/min helper/any 2) "x")]
    (is (:fail? reply))
    (is (:changed? reply))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/min helper/any 2) "xx")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x] (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/min helper/any 2) "abcdef")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\a \b \c \d \e \f] (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/min (c/char \x) 2) "xxxy")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x \x] (:value reply)))
    (is (= #{(error/expected-input \x)
             (error/unexpected-input \y)}
           (:messages reply)))))

(deftest max-test
  (let [reply (helper/run (p/max helper/any 2))]
    (is (:ok? reply))
    (is (not (:changed? reply)))
    (is (empty? (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/max helper/any 2) "x")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x] (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/max helper/any 2) "xx")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x] (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/max helper/any 2) "abcdef")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\a \b] (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/max (c/char \x) 3) "xxy")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x] (:value reply)))
    (is (= #{(error/expected-input \x)
             (error/unexpected-input \y)}
           (:messages reply)))))

(deftest repeat-test
  (let [reply (helper/run (p/repeat helper/any 2))]
    (is (:fail? reply))
    (is (not (:changed? reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/repeat helper/any 2) "x")]
    (is (:fail? reply))
    (is (:changed? reply))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/repeat helper/any 2) "xx")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x] (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/repeat helper/any 2) "xxxy")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x] (:value reply)))
    (is (nil? (:error reply))))

  (let [reply (helper/run (p/repeat helper/any 2 3) "x")]
    (is (:fail? reply))
    (is (:changed? reply))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/repeat helper/any 2 3) "xx")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x] (:value reply)))
    (is (= error/unexpected-eof (:error reply))))

  (let [reply (helper/run (p/repeat (c/char \x) 2 3) "xxxyy")]
    (is (:ok? reply))
    (is (:changed? reply))
    (is (= [\x \x \x] (:value reply)))
    (is (nil? (:error reply)))))

(deftest seqexp-test
  (let [p (p/cat (p/* (c/char \a))
                 (p/+ (c/char \b)))]
    (is (= [\b] (p/parse p "b")))
    (is (= [\b] (p/parse p "bcc")))
    (is (= [\a \b \b] (p/parse p "abb")))
    (is (= [\a \a \a \b] (p/parse p "aaabc"))))

  (let [p (p/cat (p/* (p/cat (c/char \a) (c/char \b)))
                 (p/+ (c/char \c)))]
    (is (= [\c] (p/parse p "cdef")))
    (is (= [\a \b \a \b \c] (p/parse p "ababc"))))

  (let [p (p/cat (p/* (p/group (c/char \a) (c/char \b)))
                 (p/+ (c/char \c)))]
    (is (= [[\a \b] [\a \b] \c] (p/parse p "ababc"))))

  (let [p (p/cat (p/* (c/char \a))
                 (p/max (c/char \b) 3)
                 (p/? (c/char \c))
                 (p/min (c/char \d) 2)
                 (p/repeat (c/char \e) 3)
                 (p/repeat (c/char \f) 0 3)
                 (p/+ (c/char \g)))]
    (is (:fail? (helper/run p "abcdefg")))
    (is (= [\b \d \d \e \e \e \g] (p/parse p "bddeeeg"))))

  (let [p (p/cat (p/skip+ (c/char \x)) (c/char \y) (p/skip* (c/char \z)))]
    (let [reply (helper/run p "y")]
      (is (:fail? reply))
      (is (not (:changed? reply)))
      (is (= #{(error/expected-input \x) (error/unexpected-input \y)} (:messages reply))))

    (let [reply (helper/run p "xy")]
      (is (:ok? reply))
      (is (:changed? reply))
      (is (= [\y] (:value reply)))
      (is (= #{(error/expected-input \z) error/unexpected-eof} (:messages reply))))

    (let [reply (helper/run p "xyy")]
      (is (:ok? reply))
      (is (:changed? reply))
      (is (= [\y] (:value reply)))
      (is (= #{(error/expected-input \z) (error/unexpected-input \y)} (:messages reply))))

    (let [reply (helper/run p "xxxxxxxxyzzzzzzzzz")]
      (is (:ok? reply))
      (is (= [\y] (:value reply))))))

(deftest lazy-test
  (let [a (atom 0)
        p (p/lazy (p/return @a))]
    (testing "computes parser lazily"
      (swap! a inc)
      (is (= 1 (p/parse p ""))))
    (testing "lazy parser is cached"
      (swap! a inc)
      (is (= 1 (p/parse p ""))))))

(deftest ref-test
  (let [a (atom (p/return 42))
        p (p/ref a)]
    (is (= 42 (p/parse p "test")))
    (reset! a helper/any)
    (is (= \t (p/parse p "test")))))

(deftest rec-test
  (let [p (p/rec #(p/alt (c/char \x) (p/between % (c/char \() (c/char \)))))]
    (let [reply (helper/run p "x")]
      (is (:ok? reply))
      (is (:changed? reply))
      (is (= \x (:value reply)))
      (is (nil? (:error reply))))

    (let [reply (helper/run p "(((x)))")]
      (is (:ok? reply))
      (is (:changed? reply))
      (is (= \x (:value reply)))
      (is (nil? (:error reply))))

    (let [reply (helper/run p "((x)")]
      (is (:fail? reply))
      (is (:changed? reply))
      (is (= #{(error/expected-input \)) error/unexpected-eof} (:messages reply))))))

(deftest index-test
  (is (= 0 (p/parse p/index "")))
  (is (= 3 (p/parse (p/then (p/repeat helper/any 3) p/index) "abcdef"))))

(deftest pos-test
  (is (= (pos/pos 0 0) (p/parse p/pos "")))
  (is (= (pos/pos 1 2) (p/parse (p/then (p/repeat c/any-char 6) p/pos) "abc\ndef"))))

(deftest user-state-test
  (let [p (p/then (p/set-user-state {})
                  (p/* (p/bind helper/any #(p/swap-user-state update % (fnil inc 0))))
                  p/user-state)]
    (is (= {\m 1, \i 4, \s 4, \p 2} (p/parse p "mississippi"))))

  (let [reply (helper/run (p/then (p/set-user-state 2)
                                  (p/match-user-state even?)))]
    (is (:ok? reply))
    (is (true? (:value reply))))

  (let [reply (helper/run (p/then (p/set-user-state 1)
                                  (p/match-user-state even?)))]
    (is (:fail? reply))
    (is (nil? (:error reply))))

  (is (= \x (p/parse (p/then (p/bind helper/any #(p/set-user-state %))
                             (p/match-user-state #{\x}))
                     "xyz"))))
