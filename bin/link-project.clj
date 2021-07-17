#!/usr/bin/env bb
(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {clj-commons/fs {:mvn/version "1.5.2"}}})
(require '[me.raynes.fs :as fs])

(comment api: (sym-link path target))

(def remove? {"remove" "delete" "unlink"})

(let [[project package command] *command-line-args*
      package-path (str/replace package #"\." "/")
      current-dir (io/file "..")
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
        (target-path dir)))))
