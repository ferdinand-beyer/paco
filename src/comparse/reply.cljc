(ns comparse.reply
  #?(:cljs (:require-macros comparse.reply)))

(deftype ReplyFns [ok ok! error error!])

(defn reply-fns
  ([ok error]
   (reply-fns ok ok error error))
  ([ok ok! error error!]
   (ReplyFns. ok ok! error error!)))

(defn- reply-fn-sym [key]
  (case key
    :ok '.-ok
    :ok! '.-ok!
    :error '.-error
    :error! '.-error!))

(defn- emit-fn [reply-fns binding-map k]
  (let [expr (get binding-map k k)]
    (if (keyword? expr)
      (list (reply-fn-sym expr) reply-fns)
      expr)))

(defn- emit-with [reply-fns binding-map]
  {:pre [(seq binding-map)
         (every? #{:ok :ok! :error :error!} (keys binding-map))]}
  (let [sym (gensym "r_")]
    `(let [~sym ~reply-fns]
       (ReplyFns.
        ~(emit-fn sym binding-map :ok)
        ~(emit-fn sym binding-map :ok!)
        ~(emit-fn sym binding-map :error)
        ~(emit-fn sym binding-map :error!)))))

(defmacro with
  "Returns `reply` with some handler functions rebound."
  [reply & {:as binding-map}]
  (emit-with reply binding-map))

(defn ok
  "Creates a success reply for a parser that succeeded
   *without changing its input state*."
  ([^ReplyFns reply state result]
   ((.-ok reply) state result nil))
  ([^ReplyFns reply state result errors]
   ((.-ok reply) state result errors)))

(defn ok!
  "Creates a success reply for a parser that succeeded
   *with changing its input state*."
  ([^ReplyFns reply state result]
   ((.-ok! reply) state result nil))
  ([^ReplyFns reply state result errors]
   ((.-ok! reply) state result errors)))

(defn error [^ReplyFns reply state errors]
  ((.-error reply) state errors))

(defn error! [^ReplyFns reply state errors]
  ((.-error! reply) state errors))
