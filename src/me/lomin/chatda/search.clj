(ns me.lomin.chatda.search
  ^{:doc "Concurrency is the problem of scheduling multiple largely independent
tasks onto a usually smaller set of computational resources.
Parallelism on the other hand means breaking a computational task down into several
sub-tasks that can be processed independently and whose results are combined
afterwards, upon completion."}
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols])
  (:import (java.util.concurrent TimeoutException Executors TimeUnit ScheduledExecutorService)
           (clojure.lang IPersistentStack Counted Seqable IEditableCollection ITransientCollection IMeta IObj)
           (java.util PriorityQueue Comparator)))

#_(set! *warn-on-reflection* true)

(defonce executor
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

(declare heap)

(defn ->item+priority [item]
  [item (priority item)])

(deftype PriorityQueueBuffer
  [^Comparator compare ^long n ^PriorityQueue buf]
  async-protocols/Buffer
  (full? [_] (if (neg? n) false (>= (.size buf) n)))
  (remove! [_] (.poll buf))
  (add!* [this itm] (.offer buf itm) this)
  (close-buf! [_])
  Counted
  (count [_] (.size buf)))

;; DO NOT USE Heap OUTSIDE THIS NAMESPACE!
;; It does not properly comply to the contract of the implemented
;; protocols in favor of performance optimization.
(deftype Heap
  [^Comparator compare ^PriorityQueue buf]
  Counted
  (count [_] (.size buf))
  IPersistentStack
  (peek [_] (when-let [item (.peek buf)] (->item+priority item)))
  (pop [self] (.poll buf) self)
  (cons [self item] (.offer buf item) self)
  (empty [_] (heap compare))
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
                  (map ->item+priority
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

(defn heap
  ([^Comparator compare]
   (new Heap
        compare
        (new PriorityQueue
             ^Comparator (priority-comparator compare)))))

(defmacro offer-to [first-problem next-heap chan]
  `(let [[second-problem#] (peek ~next-heap)
         offer# (when second-problem# (async/offer! ~chan second-problem#))]
     (cond
       ;; second-problem# was put onto chan, so forget about second-problem#
       (true? offer#) (recur ~first-problem (pop ~next-heap))
       ;; channel closed: stop immediately
       (false? offer#) ~first-problem
       ;; channel full or no problems left, so do not forget second-problem#
       :else (recur ~first-problem ~next-heap))))

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
          heap# (heap ~compare)]
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
    (.schedule ^ScheduledExecutorService @executor
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

(defn parallel-depth-first-search
  ([{:keys [compare] :or {compare depth-first-comparator} :as init}
    {:keys [chan-size parallelism timeout]
     :or   {chan-size   10
            parallelism 4}}]
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
   (parallel-depth-first-search init {:chan-size   chan-size
                                      :parallelism parallelism})))
