(ns me.lomin.chatda.search
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]
            [clojure.data.priority-map :as pm])
  (:import (java.util.concurrent TimeoutException)))

(set! *warn-on-reflection* true)

(defprotocol Searchable
  (children [self])
  (xform [self]))

(defprotocol AsyncSearchable
  (xform-async [self]))

(extend-type nil
  Searchable
  (children [_] nil))

(deftype PriorityQueueBuffer [^long n ^java.util.PriorityQueue buf]
  async-protocols/Buffer
  (full? [this]
    (>= (.size buf) n))
  (remove! [this]
    (.poll buf))
  (add!* [this itm]
    (.offer buf itm)
    this)
  (close-buf! [this])
  clojure.lang.Counted
  (count [this]
    (.size buf)))

(defprotocol Prioritizable
  (priority [self]))

(defn priority-comparator [compare]
  (fn [a b] (compare (priority a) (priority b))))

(defn priority-queue
  ([^long n ^java.util.Comparator compare]
   (priority-queue n compare nil))
  ([^long n ^java.util.Comparator compare init]
   (new PriorityQueueBuffer n
        (cond-> (new java.util.PriorityQueue (max 1 n)
                     ^java.util.Comparator (priority-comparator compare))
                (some? init) (doto (.add init))))))

(defprotocol ExhaustiveSearch
  (stop [this]))

(extend-protocol ExhaustiveSearch
  Object (stop [this] (reduced this))
  nil (stop [this] (reduced this)))

(defprotocol Combinable
  (combine [this other])
  (combine-async [this other]))

(extend-protocol Combinable
  Object
  (combine [_ other] other)
  (combine-async [this other] (combine this other))
  nil
  (combine [_ other] other)
  (combine-async [this other] (combine this other)))

(defmacro combine->offer->recur [problem heap chan parallel?]
  `(if (seq ~heap)
     (let [first-problem# (combine ~problem (first (peek ~heap)))
           next-heap# (pop ~heap)
           [second-problem#] (peek next-heap#)
           offer# (and second-problem# ~parallel? (async/offer! ~chan second-problem#))]
       (cond
         ;; second-problem# put onto chan: forget about second-problem#
         offer# (recur first-problem# (pop next-heap#))
         ;; channel full or no problems left: do not forget second-problem#
         (or (not ~parallel?) (nil? offer#)) (recur first-problem# next-heap#)
         ;; channel closed: stop immediately
         :else first-problem#))
     ~problem))

(defn async-worker [xform problem-bus empty-heap parallel? problem]
  (async/go
    (try
      (loop [p problem
             heap (empty-heap)]
        (if-let [$children (seq (children p))]
          (if-let [next-heap (try (into heap xform $children)
                                  (catch TimeoutException _))]
            (combine->offer->recur p next-heap problem-bus parallel?)
            p)
          (if-let [result (stop p)]
            result
            (combine->offer->recur p heap problem-bus parallel?))))
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

(defn rec:parallel-depth-first-search
  [root-problem control-chan parallelism solve-async]
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
                                        (solve-async $val)))
        :else (recur (combine-async problem $val)
                     (remove-worker-from worker-pool ch))))))

(def depth-first-comparator (fn [a b] (compare b a)))
(def priority-xf (map (fn [x] [x (priority x)])))
;; create TimeoutException ahead of time and only once, since
;; creating an exception is expensive and we are not interested
;; in the stacktrace.
(def timeout-exception (new TimeoutException))

(defn timeout-xf [timeout control-chan]
  (let [timed-out? (volatile! false)]
    (async/go (async/<! (async/timeout timeout))
              (async/close! control-chan)
              (vreset! timed-out? true))
    (map (fn [x]
           (if @timed-out?
             (throw timeout-exception)
             x)))))

(defn chan-xform [init]
  (if (satisfies? AsyncSearchable init)
    (xform-async init)
    (xform init)))

(defn parallel-depth-first-search
  ([{:keys [compare] :or {compare depth-first-comparator} :as init}
    {:keys [chan-size parallelism timeout]
     :or   {chan-size   10
            parallelism 4}}]
   (let [xf (chan-xform init)
         root-problem (transduce-1 xf init)
         control-chan (async/chan (priority-queue chan-size compare root-problem)
                                 xf)
         search-xf (if timeout
                     (comp (timeout-xf timeout control-chan) (xform init) priority-xf)
                     (comp (xform init) priority-xf))
         empty-heap (partial pm/priority-map-by compare)
         solve-async (partial async-worker
                              search-xf
                              control-chan
                              empty-heap
                              (< 1 parallelism))]
     (try
       (rec:parallel-depth-first-search root-problem
                                        control-chan
                                        parallelism
                                        solve-async)
       (finally
         (async/close! control-chan)))))
  ([init chan-size parallelism]
   (parallel-depth-first-search init {:chan-size   chan-size
                                      :parallelism parallelism})))
