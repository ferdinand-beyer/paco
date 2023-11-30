(ns paco.reply
  #?(:cljs (:require-macros [paco.reply]))
  (:require [paco.error :as error]))

(deftype Context [ok ok! fail fail!])

(defn context
  ([ok fail]
   (context ok ok fail fail))
  ([ok ok! fail fail!]
   (Context. ok ok! fail fail!)))

(defn ok
  "Succeed without changing `state`."
  ([^Context ctx state value]
   ((.-ok ctx) state value nil))
  ([^Context ctx state value msg]
   ((.-ok ctx) state value msg)))

(defn ok!
  "Succeed with changing `state`."
  ([^Context ctx state value]
   ((.-ok! ctx) state value nil))
  ([^Context ctx state value msg]
   ((.-ok! ctx) state value msg)))

(defn fail
  "Fail without changing `state`."
  [^Context ctx state msg]
  ((.-fail ctx) state msg))

(defn fail!
  "Fail with changing `state`."
  [^Context ctx state msg]
  ((.-fail! ctx) state msg))

(defn- emit-ctx-get [ctx k]
  (-> #?(:bb k
         :default (case k
                    :ok    '.-ok
                    :ok!   '.-ok!
                    :fail  '.-fail
                    :fail! '.-fail!))
      (list ctx)))

(defn- emit-with-arg [ctx binding-map k]
  (let [expr (get binding-map k k)]
    (if (keyword? expr)
      (emit-ctx-get ctx expr)
      expr)))

(defn- emit-with [ctx bindings]
  (if (seq bindings)
    (let [binding-map (apply hash-map bindings)
          hinted-ctx  (with-meta (gensym "ctx_") {:tag `Context})]
      `(let [~hinted-ctx ~ctx]
         (#?(:bb ->Context, :default Context.)
          ~(emit-with-arg hinted-ctx binding-map :ok)
          ~(emit-with-arg hinted-ctx binding-map :ok!)
          ~(emit-with-arg hinted-ctx binding-map :fail)
          ~(emit-with-arg hinted-ctx binding-map :fail!))))
    ctx))

(defmacro with
  "Returns `ctx` with updated continuation functions."
  {:style/indent 1}
  [ctx & bindings]
  (emit-with ctx bindings))

(defn- emit-continue-thunk [p state ctx]
  `(let [ctx# ~ctx]
     (fn ~'cont [] (~p ~state ctx#))))

(defn- emit-continue [p state ctx bindings]
  (emit-continue-thunk p state (emit-with ctx bindings)))

(defmacro continue
  "Delegate to parser `p`, returning a thunk that will call
   `(p state ctx)`.  Accepts args to update `ctx` using `with`."
  {:style/indent 3}
  [p state ctx & bindings]
  (emit-continue p state ctx bindings))

(comment
  (emit-with 'ctx [])
  (emit-with 'ctx [:ok :ok!])

  (emit-continue 'p 'state 'ctx [:ok :ok!])
  ;
  )

(defn fwd-ok [^Context ctx msg]
  (fn [state value msg2]
    ((.-ok ctx) state value (error/union msg msg2))))

(defn fwd-ok! [^Context ctx msg]
  (fn [state value msg2]
    ((.-ok! ctx) state value (error/union msg msg2))))

(defn fwd-fail [^Context ctx msg]
  (fn [state msg2]
    ((.-fail ctx) state (error/union msg msg2))))

(defn fwd-fail! [^Context ctx msg]
  (fn [state msg2]
    ((.-fail! ctx) state (error/union msg msg2))))
