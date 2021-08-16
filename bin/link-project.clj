#!/usr/bin/env bb
(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {clj-commons/fs {:mvn/version "1.5.2"}}})
(require '[me.raynes.fs :as fs])
(require '[clojure.java.shell :refer [sh]])
(require '[clojure.string :as string])
(require '[clojure.edn :as edn])

(comment api (sym-link path target))

(def command (first *command-line-args*))

(def remove? #{"remove" "delete" "unlink"})

(defn sh-run!
  ([args] (sh-run! args (partial apply println)))
  ([args f]
   (let [result (apply sh args)]
     (f (sequence (remove string/blank?)
                  [(:out result) (:err result)]))
     result)))

(defn parse-dogu-edn []
  (let [{:keys [out exit]} (sh-run! ["cat" "dogu.edn"] identity)]
    (when (zero? exit)
      (:link (edn/read-string out)))))

(for [{:keys [package project]} (parse-dogu-edn)]
  (let [package-path (str/replace package #"\." "/")
        current-dir (io/file ".")
        source-dir (io/file project)]

    (defn link-here-path [dir]
      (str (.getCanonicalPath current-dir) "/" dir "/" package-path))

    (defn target-path [dir]
      (str (.getCanonicalPath source-dir) "/" dir "/" package-path))

    (if (remove? command)
      (doseq [dir ["src" "test"]]
        (fs/delete (link-here-path dir)))
      (doseq [dir ["src" "test"]]
        (fs/sym-link
          (link-here-path dir)
          (target-path dir))))))
