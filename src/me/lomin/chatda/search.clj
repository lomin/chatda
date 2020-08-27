(ns me.lomin.chatda.search
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]
            [clojure.data.priority-map :as pm]))

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

(defn priority-queue [^long n ^java.util.Comparator compare]
  (new PriorityQueueBuffer n
       (new java.util.PriorityQueue n
            ^java.util.Comparator (priority-comparator compare))))

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

(defmacro combine->offer->recur [problem heap chan]
  `(if (seq ~heap)
     (let [$first# (combine ~problem (first (peek ~heap)))
           $next# (pop ~heap)
           [spawn#] (peek $next#)]
       (if (async-protocols/closed? ~chan)
         $first#
         (recur $first#
                (if (and spawn# (async/offer! ~chan spawn#))
                  (pop $next#)
                  $next#))))
     ~problem))

(defn add-async-worker-to [worker-pool problem xform chan compare]
  (->> (async/go-loop [p problem
                       heap (pm/priority-map-by compare)]
         (if-let [$children (seq (children p))]
           (let [next-heap (into heap xform $children)]
             (combine->offer->recur p next-heap chan))
           (if-let [result (stop p)]
             result
             (combine->offer->recur p heap chan))))
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
  [worker-pool control-chans parallelism]
  (if (<= parallelism (count worker-pool))
    (async/alts!! (into worker-pool (rest control-chans)))
    (if (seq worker-pool)
      (async/alts!! (into worker-pool control-chans))
      (async/alts!! control-chans :default nil))))

(defn rec:parallel-depth-first-search
  [root-problem xform [main-chan timeout-chan :as control-chans] parallelism compare]
  (loop [problem root-problem
         worker-pool []]
    (let [[$val ch] (next-channel-value worker-pool control-chans parallelism)]
      (cond
        (reduced? $val) @$val
        (= ch main-chan) (recur problem
                                (add-async-worker-to worker-pool
                                                     $val xform main-chan compare))
        (= ch :default) problem
        (= ch timeout-chan) problem
        :else (recur (combine problem $val)
                     (remove-worker-from worker-pool ch))))))

(def priority-xf (map (fn [x] [x (priority x)])))
(def depth-first-comparator (fn [a b] (compare b a)))

(defn parallel-depth-first-search
  ([{:keys [compare] :or {compare depth-first-comparator} :as init}
    {:keys [chan-size parallelism timeout]
     :or   {chan-size   10
            parallelism 4}}]
   (let [xform (xform init)
         root-problem (transduce-1 xform init)
         chan (async/chan (priority-queue chan-size compare))
         close-chan-future (when timeout (future (Thread/sleep timeout)
                                                 (async/close! chan)))
         control-chans (cond-> [chan]
                               timeout (conj (async/timeout timeout)))]
     (async/>!! chan root-problem)
     (let [result (rec:parallel-depth-first-search root-problem
                                                   (comp xform priority-xf)
                                                   control-chans
                                                   parallelism
                                                   compare)]
       (async/close! chan)
       (when timeout (future-cancel close-chan-future))
       result)))
  ([init chan-size parallelism]
   (parallel-depth-first-search init {:chan-size   chan-size
                                      :parallelism parallelism})))
