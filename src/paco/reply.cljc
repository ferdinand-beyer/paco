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
  ([^Context ctx state value]
   ((.-ok ctx) state value nil))
  ([^Context ctx state value msg]
   ((.-ok ctx) state value msg)))

(defn ok!
  ([^Context ctx state value]
   ((.-ok! ctx) state value nil))
  ([^Context ctx state value msg]
   ((.-ok! ctx) state value msg)))

(defn fail
  [^Context ctx state msg]
  ((.-fail ctx) state msg))

(defn fail!
  [^Context ctx state msg]
  ((.-fail! ctx) state msg))

(defn- ctx-field [key]
  (case key
    :ok    '.-ok
    :ok!   '.-ok!
    :fail  '.-fail
    :fail! '.-fail!))

(defn- emit-ctx-get [ctx k]
  (list (ctx-field k) ctx))

(defn- emit-ctx-arg [ctx binding-map k]
  (let [expr (get binding-map k k)]
    (if (keyword? expr)
      (emit-ctx-get ctx expr)
      expr)))

(defn- emit-with [ctx bindings]
  (if (seq bindings)
    (let [binding-map (apply hash-map bindings)
          ctx'        (with-meta (gensym "ctx_") {:tag `Context})]
      `(let [~ctx' ~ctx]
         (Context.
          ~(emit-ctx-arg ctx' binding-map :ok)
          ~(emit-ctx-arg ctx' binding-map :ok!)
          ~(emit-ctx-arg ctx' binding-map :fail)
          ~(emit-ctx-arg ctx' binding-map :fail!))))
    ctx))

(defmacro with
  {:style/indent 1}
  [ctx & bindings]
  (emit-with ctx bindings))

(defn- emit-continue-thunk [p state ctx]
  `(let [ctx# ~ctx]
     (fn ~'cont [] (~p ~state ctx#))))

(defn- emit-continue [p state ctx bindings]
  (emit-continue-thunk p state (if (seq bindings)
                                 (emit-with ctx bindings)
                                 ctx)))

(defmacro continue
  {:style/indent 3}
  [p state ctx & bindings]
  (emit-continue p state ctx bindings))

(comment
  (emit-with 'ctx [])
  (emit-with 'ctx [:ok :ok!])

  (emit-continue 'p 'state 'ctx [:ok :ok!])
  ;
  )

(defn fwd-ok [ctx msg]
  (fn [state value msg2]
    (ok ctx state value (error/union msg msg2))))

(defn fwd-ok! [ctx msg]
  (fn [state value msg2]
    (ok! ctx state value (error/union msg msg2))))

(defn fwd-fail [ctx msg]
  (fn [state msg2]
    (fail ctx state (error/union msg msg2))))

(defn fwd-fail! [ctx msg]
  (fn [state msg2]
    (fail! ctx state (error/union msg msg2))))
