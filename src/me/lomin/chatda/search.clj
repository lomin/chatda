(ns me.lomin.chatda.search
  ^{:doc "Concurrency is the problem of scheduling multiple largely independent
tasks onto a usually smaller set of computational resources.
Parallelism on the other hand means breaking a computational task down into several
sub-tasks that can be processed independently and whose results are combined
afterwards, upon completion."}
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]
            [clojure.core.async.impl.concurrent :as conc])
  (:import (java.util.concurrent TimeoutException Executors TimeUnit ScheduledExecutorService)
           (clojure.lang IPersistentStack Counted Seqable IEditableCollection ITransientCollection IMeta IObj)
           (java.util PriorityQueue Comparator)
           (clojure.core.async.impl.protocols Executor)))

#_(set! *warn-on-reflection* true)

(defn make-switching-executor [delayed-core-async-executor]
  (delay (let [core-async-executor @delayed-core-async-executor
               opts {:init-fn
                     #(.set ^ThreadLocal @#'clojure.core.async.impl.dispatch/in-dispatch true)}
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

(defonce init-custom-thread-pool-executor
         (delay (alter-var-root #'clojure.core.async.impl.dispatch/executor
                                make-switching-executor)))

(defonce timeout-executor
         (delay (Executors/newSingleThreadScheduledExecutor)))

(defprotocol Searchable
  (children [self])
  (xform [self])
  (priority [self])
  (stop [this children])
  (combine [this other]))

(defprotocol AsyncSearchable
  (xform-async [self])
  (combine-async [this other]))

(deftype PriorityQueueBuffer
  [^Comparator compare ^long n ^PriorityQueue buf]
  async-protocols/Buffer
  (full? [_] (if (neg? n) false (>= (.size buf) n)))
  (remove! [_] (.poll buf))
  (add!* [this itm] (.offer buf itm) this)
  (close-buf! [_])
  Counted
  (count [_] (.size buf)))

(declare make-heap)
(defn heap-item->item+priority [item] [item (priority item)])

;; DO NOT USE Heap OUTSIDE THIS NAMESPACE!
;; It does not properly comply to the contract of the implemented
;; protocols in favor of performance optimization. The use of Heap in this
;; namespace is intended to look like idiomatic Clojure code, but if it is
;; used other than the fine-tuned accesses in this namespace, the abstraction
;; will probably break.
;; Heap behaves like a mutable variable with the API of a transient, but without
;; its semantic. This is reflected in that calling (persistent! heap), the same
;; instance of Heap will be returned.
;; Neither Heap nor its underlying java.util.PriorityQueue are synchronized.
;; This is fine in this namespace, since we guarantee that a Heap has only
;; exactly one accessor, i.e. we guarantee thread isolation.
;; Why do implement our heap, if there is `clojure.data.priority-map`?
;; Because `clojure.data.priority-map`
(deftype Heap
  [^Comparator compare ^PriorityQueue buf]
  Counted
  (count [_] (.size buf))
  IPersistentStack
  (peek [_] (when-let [item (.peek buf)] (heap-item->item+priority item)))
  (pop [self] (.poll buf) self)
  (cons [self item] (.offer buf item) self)
  (empty [_] (make-heap compare))
  (equiv [self other] (identical? self other))
  IEditableCollection
  (asTransient [self] self)
  ITransientCollection
  (conj [self item] (.offer buf item) self)
  (persistent [self] self)
  IMeta
  (meta [_] {})
  IObj
  (withMeta [self _] self)
  Seqable
  (seq [self]
    (when-let [head (peek self)]
      (cons head
            (when (< 1 (.count self))
              (lazy-seq
                (let [iterator (.iterator buf)]
                  (.next iterator)
                  (map heap-item->item+priority
                       (iterator-seq iterator)))))))))

(defn priority-comparator [compare]
  (fn [a b] (compare (priority a) (priority b))))

(defn priority-queue
  ([^Comparator compare]
   (priority-queue compare -1))
  ([^Comparator compare ^long n]
   (priority-queue compare n nil))
  ([^Comparator compare ^long n init]
   (new PriorityQueueBuffer
        compare
        n
        (cond-> (new PriorityQueue (max 1 n)
                     ^Comparator (priority-comparator compare))
                (some? init) (doto (.add init))))))

(defn make-heap
  ([^Comparator compare]
   (new Heap
        compare
        (new PriorityQueue
             ^Comparator (priority-comparator compare)))))

(defmacro offer-to [first-problem next-heap chan]
  `(let [[second-problem#] (peek ~next-heap)
         offer# (when second-problem# (async/offer! ~chan second-problem#))]
     (cond
       ;; channel full or no problems left, so do not forget second-problem#
       (nil? offer#) (recur ~first-problem ~next-heap)
       ;; second-problem# was put onto chan, so forget about second-problem#
       (true? offer#) (recur ~first-problem (pop ~next-heap))
       ;; channel closed: stop immediately
       (false? offer#) ~first-problem)))

(defmacro combine->recur [problem heap & [f & more :as body]]
  (let [first-problem (gensym)
        next-heap (gensym)
        body (if (seq body)
               (cons f (cons first-problem (cons next-heap more)))
               `(recur ~first-problem ~next-heap))]
    `(if-let [heap-head# (peek ~heap)]
       (let [~first-problem (combine ~problem (first heap-head#))
             ~next-heap (pop ~heap)]
         ~body)
       ~problem)))

(defmacro worker [problem search-xf compare & args]
  `(loop [p# ~problem
          heap# (make-heap ~compare)]
     (let [children# (children p#)]
       (if-let [result# (stop p# children#)]
         result#
         (if-let [next-heap# (try (into heap# ~search-xf children#)
                                  (catch TimeoutException _#))]
           (combine->recur p# next-heap# ~@args)
           p#)))))

(defn async-worker [problem {:keys [search-xf control-chan compare]}]
  (async/go
    (try
      (worker problem search-xf compare offer-to control-chan)
      (catch Exception e e))))

(defn transduce-1
  "apply a transducer on a single value"
  [xform x]
  (first (into [] xform [x])))

(defn remove-worker-from [worker-pool worker]
  (filterv #(not= % worker) worker-pool))

(defn next-channel-value
  [worker-pool control-chan parallelism]
  (if (<= parallelism (count worker-pool))
    (async/alts!! worker-pool)
    (if (seq worker-pool)
      (async/alts!! (conj worker-pool control-chan))
      [(async/poll! control-chan) control-chan])))

(defn search-parallel
  [{:keys [root-problem control-chan parallelism] :as config}]
  (loop [problem root-problem
         worker-pool []]
    (let [[$val ch] (next-channel-value worker-pool
                                        control-chan parallelism)]
      (cond
        (nil? $val) problem
        (reduced? $val) @$val
        (instance? Throwable $val) (throw $val)
        (= ch control-chan) (recur problem
                                   (conj worker-pool
                                         (async-worker $val config)))
        :else (recur (combine-async problem $val)
                     (remove-worker-from worker-pool ch))))))

(defn search-sequential [{:keys [root-problem search-xf compare]}]
  (let [result (worker root-problem search-xf compare)]
    (if (reduced? result)
      @result
      result)))

;; (compare a b) returns a comparator that favors the smaller element,
;; but if we want to favor an element deeper in the search space (further
;; from the root, i.e. with a higher depth value), we have to switch the arguments:
(def depth-first-comparator (fn [a b] (compare b a)))

;; create TimeoutException ahead of time and only once, since
;; creating an exception is expensive and we are not interested
;; in the stacktrace.
(def timeout-exception (new TimeoutException))

(defn maybe-schedule [^Runnable f timeout]
  (when timeout
    (.schedule ^ScheduledExecutorService @timeout-executor
               ^Runnable f
               ^long timeout
               TimeUnit/MILLISECONDS)))

(defn timeout-xf [timed-out?]
  (map (fn [x]
         (if @timed-out?
           (throw timeout-exception)
           x))))

(defn chan-xform [init]
  (if (satisfies? AsyncSearchable init)
    (xform-async init)
    (map identity)))

(defn check-config! [problem parallel? parallelism]
  (if (and parallel?
           (not (satisfies? AsyncSearchable problem)))
    (throw (new IllegalArgumentException
                ^Throwable
                (ex-info "Problems that do not implement AsyncSearchable
                  must be searched sequential"
                         {:parallelism parallelism
                          :problem     problem})))))

(defn search
  ([{:keys [compare] :or {compare depth-first-comparator} :as init}
    {:keys [chan-size parallelism timeout]
     :or   {chan-size   10
            parallelism 4}}]
   @init-custom-thread-pool-executor
   (let [parallel? (< 1 parallelism)
         _ (check-config! init parallel? parallelism)
         xf (chan-xform init)
         root-problem (transduce-1 xf init)
         control-chan (when parallel?
                        (async/chan (priority-queue compare chan-size root-problem)
                                    xf))
         timed-out? (when timeout (volatile! false))
         maybe-future (maybe-schedule #(do (when control-chan (async/close! control-chan))
                                           (vreset! timed-out? true))
                                      timeout)
         search-xf (if timeout
                     (comp (timeout-xf timed-out?) (xform init))
                     (comp (xform init)))
         config {:root-problem root-problem
                 :control-chan control-chan
                 :search-xf    search-xf
                 :parallelism  parallelism
                 :compare      compare}]
     (try
       (if parallel?
         (search-parallel config)
         (search-sequential config))
       (finally
         (when maybe-future (future-cancel maybe-future))
         (when control-chan (async/close! control-chan))))))
  ([init chan-size parallelism]
   (search init {:chan-size   chan-size
                 :parallelism parallelism})))
