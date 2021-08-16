(ns me.lomin.chatda.threadpool
  ^{:doc "Different thread-pools for:
           1: search work -> cpu-bound thread-pool with size = available processors
           2: timeouts    -> single-thread thread-pool
           3: concurrency -> core.async thread-pool"}
  (:require [clojure.core.async.impl.concurrent :as conc]
            [clojure.core.async.impl.protocols :as async-protocols]
            [clojure.core.async.impl.dispatch :as async-dispatch])
  (:import (java.util.concurrent Executors)
           (clojure.core.async.impl.protocols Executor)))

(defn make-switching-executor [delayed-core-async-executor]
  (delay (let [core-async-executor @delayed-core-async-executor
               opts {:init-fn
                     #(.set ^ThreadLocal @#'async-dispatch/in-dispatch true)}
               cpu-bound-executor (Executors/newFixedThreadPool
                                    (.availableProcessors (Runtime/getRuntime))
                                    (conc/counted-thread-factory "me.lomin.chatda.search/async-worker-%d"
                                                                 true
                                                                 opts))]
           (reify Executor
             (async-protocols/exec [_ runnable]
               (if (= (.getPackageName (.getClass runnable)) "me.lomin.chatda")
                 (.execute cpu-bound-executor ^Runnable runnable)
                 (async-protocols/exec core-async-executor runnable)))))))

(defonce custom-thread-pool-executor
         (delay (alter-var-root #'clojure.core.async.impl.dispatch/executor
                                make-switching-executor)))

(defonce timeout-executor
         (delay (Executors/newSingleThreadScheduledExecutor)))