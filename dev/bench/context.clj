(ns bench.context
  (:require [criterium.core :as criterium]))

;;---------------------------------------------------------
;; Passing a context/env record

(defrecord Context [ok ok! fail fail!])

(defn ok [^Context ctx state value msgs]
  ((.ok ctx) state value msgs))

(defn ok! [^Context ctx state value msgs]
  ((.ok! ctx) state value msgs))

(defn fail [^Context ctx state msgs]
  ((.fail ctx) state msgs))

(defn fail! [^Context ctx state msgs]
  ((.fail! ctx) state msgs))

(defn- emit-context-accessor [key]
  (case key
    :ok '.-ok
    :ok! '.-ok!
    :fail '.-fail
    :fail! '.-fail!))

(defn- emit-cont [context binding-map k]
  (let [expr (get binding-map k k)]
    (if (keyword? expr)
      (list (emit-context-accessor expr) context)
      expr)))

(defn- emit-with [context binding-map]
  {:pre [(seq binding-map)
         (every? #{:ok :ok! :fail :fail!} (keys binding-map))]}
  (let [ctx (with-meta (gensym "ctx_") {:tag `Context})]
    `(let [~ctx ~context]
       (Context.
        ~(emit-cont ctx binding-map :ok)
        ~(emit-cont ctx binding-map :ok!)
        ~(emit-cont ctx binding-map :fail)
        ~(emit-cont ctx binding-map :fail!)))))

(comment
  (emit-with 'the-ctx {:ok :ok! :fail '(fn [s msgs])})

  ;;
  )

(defmacro with [context & {:as binding-map}]
  (emit-with context binding-map))

(defn return1 [x]
  (fn [state ctx]
    (ok ctx state x nil)))

(defn bind1 [p f]
  (fn [state ctx]
    (letfn [(ok' [state' value msgs]
              (let [p2 (f value)
                    ctx2 (if msgs
                           (with ctx
                             :ok (fn [s v m]
                                   (ok ctx s v (concat msgs m)))
                             :fail (fn [s m]
                                     (fail ctx s (concat msgs m))))
                           ctx)]
                (fn [] (p2 state' ctx2))))
            (ok!' [state' value msgs]
              (let [p2 (f value)
                    ctx2 (if msgs
                           (with ctx
                             :ok (fn [s v m]
                                   (ok! ctx s v (concat msgs m)))
                             :fail (fn [s m]
                                     (fail! ctx s (concat msgs m))))
                           (with ctx :ok :ok! :fail :fail!))]
                (fn [] (p2 state' ctx2))))]
      (let [ctx' (with ctx :ok ok' :ok! ok!')]
        (fn [] (p state ctx'))))))

(defn run1 [p]
  (letfn [(ok [_s v _m] v)
          (fail [_s m] m)]
    (trampoline #(p :state1 (Context. ok ok fail fail)))))

;;---------------------------------------------------------
;; Passing continuation functions

(defn return2 [x]
  (fn [state ok _ _ _]
    (ok state x nil)))

(defn bind2 [p f]
  (fn [state ok ok! fail fail!]
    (letfn [(ok' [state' value msgs]
              (let [p2 (f value)
                    [ok fail] (if msgs
                                [(fn [s v m]
                                   (ok s v (concat msgs m)))
                                 (fn [s m]
                                   (fail s (concat msgs m)))]
                                [ok fail])]
                (fn [] (p2 state' ok ok! fail fail!))))
            (ok!' [state' value msgs]
              (let [p2 (f value)
                    [ok fail] (if msgs
                                [(fn [s v m]
                                   (ok! s v (concat msgs m)))
                                 (fn [s m]
                                   (fail! s (concat msgs m)))]
                                [ok! fail!])]
                (fn [] (p2 state' ok ok! fail fail!))))]
      (fn [] (p state ok' ok!' fail fail!)))))

(defn run2 [p]
  (letfn [(ok [_s v _m] v)
          (fail [_s m] m)]
    (trampoline #(p :state2 ok ok fail fail))))

(comment
  (run1 (bind1 (return1 :foo) (fn [x] (return1 [x :bar]))))
  (run2 (bind2 (return2 :foo) (fn [x] (return2 [x :bar]))))

  (do
    (println "run1")
    (criterium/bench
     (run1 (bind1 (return1 [:foo])
                  (fn [x]
                    (bind1
                     (return1 (cons :bar x))
                     (fn [y]
                       (return1 (cons :buzz y))))))))
    ;;           Execution time mean : 87,993536 ns
    ;;  Execution time std-deviation : 0,340811 ns
    ;; Execution time lower quantile : 87,385304 ns ( 2,5%)
    ;; Execution time upper quantile : 88,634038 ns (97,5%)

    (println "run2")
    (criterium/bench
     (run2 (bind2 (return2 [:foo])
                  (fn [x]
                    (bind2
                     (return2 (cons :bar x))
                     (fn [y]
                       (return2 (cons :buzz y))))))))
    ;;           Execution time mean : 91,990108 ns
    ;;  Execution time std-deviation : 0,357706 ns
    ;; Execution time lower quantile : 91,578882 ns ( 2,5%)
    ;; Execution time upper quantile : 92,832029 ns (97,5%)
    )

  (let [p1 (bind1 (return1 [:foo])
                  (fn [x]
                    (bind1
                     (return1 (cons :bar x))
                     (fn [y]
                       (return1 (cons :buzz y))))))
        p2 (bind2 (return2 [:foo])
                  (fn [x]
                    (bind2
                     (return2 (cons :bar x))
                     (fn [y]
                       (return2 (cons :buzz y))))))]
    (println "run1")
    (criterium/bench
     (run1 p1))
    ;;           Execution time mean : 87,931946 ns
    ;;  Execution time std-deviation : 0,277282 ns
    ;; Execution time lower quantile : 87,415524 ns ( 2,5%)
    ;; Execution time upper quantile : 88,503444 ns (97,5%)

    (println "run2")
    (criterium/bench
     (run2 p2))
    ;;           Execution time mean : 94,119915 ns
    ;;  Execution time std-deviation : 0,358358 ns
    ;; Execution time lower quantile : 93,557404 ns ( 2,5%)
    ;; Execution time upper quantile : 95,065699 ns (97,5%)
    )

  ;;
  )
