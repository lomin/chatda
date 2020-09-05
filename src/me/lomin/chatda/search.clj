(ns me.lomin.chatda.search
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]
            [clojure.data.priority-map :as pm])
  (:import (java.util.concurrent TimeoutException)))

(set! *warn-on-reflection* true)

(defprotocol Searchable
  (children [self])
  (xform [self]))

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
  (combine [this other]))

(extend-protocol Combinable
  Object (combine [_ other] other)
  nil (combine [_ other] other))

(defmacro combine->offer->recur [problem heap chan parallel?]
  `(if (seq ~heap)
     (let [$first# (combine ~problem (first (peek ~heap)))
           $next# (pop ~heap)
           [spawn#] (peek $next#)]
       (recur $first#
              (if (and ~parallel? spawn# (async/offer! ~chan spawn#))
                (pop $next#)
                $next#)))
     ~problem))

(defn add-async-worker-to [worker-pool problem xform problem-bus compare parallel?]
  (->> (async/go
         (try
           (loop [p problem
                  heap (pm/priority-map-by compare)]
             (if-let [$children (seq (children p))]
               (if-let [next-heap (try (into heap xform $children)
                                       (catch TimeoutException _))]
                 (combine->offer->recur p next-heap problem-bus parallel?)
                 p)
               (if-let [result (stop p)]
                 result
                 (combine->offer->recur p heap problem-bus parallel?))))
           (catch Exception e e)))
       (conj worker-pool)))

(defn transduce-1
  "apply a transducer on a single value"
  [xform x]
  (let [f (xform
            (fn [a b]
              (if (nil? a)
                [b]
                (vary-meta (conj a b) assoc ::skip-unpack? true))))]
    (let [result (f nil x)]
      (if (::skip-unpack? (meta result))
        result
        (first result)))))

(defn remove-worker-from [worker-pool worker]
  (filterv #(not= % worker) worker-pool))

(defn next-channel-value
  [worker-pool [problem-bus :as control-chans] parallelism]
  (if (<= parallelism (count worker-pool))
    (async/alts!! worker-pool)
    (if (seq worker-pool)
      (async/alts!! (conj worker-pool problem-bus))
      (async/alts!! control-chans :default nil))))

(defn rec:parallel-depth-first-search
  [root-problem xform [problem-bus :as control-chans] parallelism compare]
  (let [parallel? (< 1 parallelism)]
    (loop [problem root-problem
           worker-pool []]
      (let [[$val ch] (next-channel-value worker-pool control-chans parallelism)]
        (cond
          (reduced? $val) @$val
          (instance? Throwable $val) (throw $val)
          (= ch problem-bus) (recur problem
                                    (add-async-worker-to worker-pool
                                                         $val xform problem-bus compare parallel?))
          (= ch :default) problem
          :else (recur (combine problem $val)
                       (remove-worker-from worker-pool ch)))))))

(def depth-first-comparator (fn [a b] (compare b a)))
(def priority-xf (map (fn [x] [x (priority x)])))
(def timeout-exception (new TimeoutException))

(defn timeout-xf [$future]
  (map (fn [x]
         (if (future-done? $future)
           (throw timeout-exception)
           x))))

(defn parallel-depth-first-search
  ([{:keys [compare] :or {compare depth-first-comparator} :as init}
    {:keys [chan-size parallelism timeout]
     :or   {chan-size   10
            parallelism 4}}]
   (let [xform (xform init)
         root-problem (transduce-1 xform init)
         problem-bus (async/chan (priority-queue chan-size compare root-problem))
         control-chans [problem-bus]
         close-chan-future (when timeout (future (Thread/sleep timeout)))
         search-xf (cond-> (list xform priority-xf)
                           timeout (conj (timeout-xf close-chan-future))
                           :always (->> (apply comp)))
         result (rec:parallel-depth-first-search root-problem
                                                 search-xf
                                                 control-chans
                                                 parallelism
                                                 compare)]
     (async/close! problem-bus)
     (when timeout (future-cancel close-chan-future))
     result))
  ([init chan-size parallelism]
   (parallel-depth-first-search init {:chan-size   chan-size
                                      :parallelism parallelism})))
