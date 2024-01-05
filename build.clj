(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.fbeyer/paco)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes/clj")
(def java-class-dir "target/classes/java")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:basis basis
            :src-dirs ["src"]
            :class-dir java-class-dir
            :javac-opts ["-Xlint:-options" "--release" "8"]}))

(defn compile [_]
  (b/compile-clj {:basis basis
                  :class-dir class-dir
                  :compile-opts {:direct-linking true}}))

(defn decompile [_]
  ;; https://vineflower.org/usage/
  (let [args (into-array String ["--folder" "-log=WARN" class-dir class-dir])]
    (org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler/main args)))

(defn recompile [params]
  (clean params)
  (compile params)
  (decompile params))
