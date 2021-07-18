#!/usr/bin/env bb
(require '[babashka.deps :as deps])
(require '[clojure.java.shell :refer [sh]])
(require '[clojure.string :as string])
(require '[clojure.edn :as edn])
(require '[babashka.process :as p])

(def exclusions #{".*/" "\\.*" "__pycache__/"
                  "venv/" "shadow-cljs.edn" "package.json"
                  "package-lock.json" "node_modules" "target"
                  "generated/*" '"*\\.iml"})

(defn rsync [src dest target user]
  (-> ["rsync" "-azP" "--copy-links" "--delete"]
      (into (interleave (repeat "--exclude") exclusions))
      (into [src (str user "@" dest ":/home/" user target)])))

(defn sh-run!
  ([args] (sh-run! args (partial apply println)))
  ([args f]
   (let [result (apply sh args)]
     (f (sequence (remove string/blank?)
                  [(:out result) (:err result)]))
     result)))

(defn parse-deps-edn []
  (let [{:keys [out exit]} (sh-run! ["cat" "deps.edn"] identity)]
    (when (zero? exit)
      (:sync (edn/read-string out)))))

(defn parse-project-clj []
  (let [{:keys [out exit]} (sh-run! ["cat" "project.clj"] identity)]
    (when (zero? exit)
      (second (drop-while #(not= :sync %) (edn/read-string out))))))

(defn watch! [[src dest target user]]
  (do
    (println "syncing" src "to" dest)
    (when (zero? (:exit (sh-run! (rsync src dest target user))))
      @(p/process ["bash" "-c"
                   (str "fswatch -o " src
                        " |  xargs -n1 -I {} "
                        *file* " " src " " dest " " target " " user " " "watching")]
                  {:inherit  true
                   :shutdown p/destroy-tree}))))

(defn rsync! [[src dest target user]]
  (do (sh-run! (rsync src dest target user)) nil))

(defn invalid-flag! [flag]
  (do (println "invalid flag:" flag)
      (System/exit 1)))

(defn sync-config-incomplete! [[src dest target user]]
  (do (println "sync config incomplete:"
               (zipmap '[src dest target user "init"]
                       [src dest target user "init"]))
      (System/exit 1)))

(defn no-sync-config-found! []
  (do (println "no sync config found in deps.edn or project.clj")
      (System/exit 1)))

(defn main [[_ _ _ _ flag :as args]]
  (if (seq args)
    (cond
      (= flag "init") (watch! args)
      (= flag "watching") (rsync! args)
      :else (invalid-flag! flag))
    (if-let [{:keys [src dest target user]}
             (or (parse-deps-edn)
                 (parse-project-clj))]
      (let [args [src dest target user "init"]]
        (if (every? some? args)
          (recur args)
          (sync-config-incomplete! args)))
      (no-sync-config-found!))))

(main *command-line-args*)