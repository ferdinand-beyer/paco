(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.fbeyer/paco)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile [_]
  (b/compile-clj {:basis basis
                  :class-dir class-dir
                  :compile-opts {:direct-linking true}}))

(defn decompile [_]
  ;; https://vineflower.org/usage/
  (let [args (into-array String ["--folder" "-log=WARN" class-dir class-dir])]
    (org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler/main args)))
