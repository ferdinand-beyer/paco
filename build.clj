(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.fbeyer/paco)
(def version (format "0.3.%s" (b/git-count-revs nil)))

(def class-dir "target/classes/clj")
(def java-class-dir "target/classes/java")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def basis (delay (b/create-basis {:project "deps.edn"})))

(def pom-data
  [[:description "Parser combinators for Clojure(Script)"]
   [:url "https://github.com/ferdinand-beyer/paco"]
   [:licenses
    [:license
     [:name "MIT License"]
     [:url "https://opensource.org/license/mit"]]]
   [:developers
    [:developer
     [:name "Ferdinand Beyer"]]]
   [:scm
    [:url "https://github.com/ferdinand-beyer/paco"]
    [:connection "scm:git:https://github.com/ferdinand-beyer/paco.git"]
    [:developerConnection "scm:git:ssh:git@github.com:ferdinand-beyer/paco.git"]
    [:tag (str "v" version)]]])

(defn clean [_]
  (b/delete {:path "target"}))

(defn javac [_]
  (b/javac {:basis @basis
            :src-dirs ["src"]
            :class-dir java-class-dir
            :javac-opts ["-Xlint:all" "-Xlint:-options" "--release" "8"]}))

(defn compile [_]
  (b/compile-clj {:basis @basis
                  :class-dir class-dir
                  :compile-opts {:direct-linking true}}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data pom-data})
  (b/copy-dir {:src-dirs ["src" "resources" java-class-dir]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy [_]
  (let [deploy (requiring-resolve 'deps-deploy.deps-deploy/deploy)]
    (deploy {:installer :remote
             :artifact  (b/resolve-path jar-file)
             :pom-file  (b/pom-path {:class-dir class-dir
                                     :lib lib})})))

(defn decompile [_]
  ;; https://vineflower.org/usage/
  (let [args (into-array String ["--folder" "-log=WARN" class-dir class-dir])]
    (org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler/main args)))

(defn recompile [params]
  (clean params)
  (javac params)
  (compile params)
  (decompile params))
