(ns me.lomin.chatda.search
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]
            [clojure.data.priority-map :as pm])
  (:import (java.util.concurrent TimeoutException)))

#_(set! *warn-on-reflection* true)

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

(defmacro offer-to [first-problem next-heap chan]
  `(let [[second-problem#] (peek ~next-heap)
         offer# (and second-problem# (async/offer! ~chan second-problem#))]
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
    `(if (seq ~heap)
       (let [~first-problem (combine ~problem (first (peek ~heap)))
             ~next-heap (pop ~heap)]
         ~body)
       ~problem)))

(defmacro worker [problem search-xf compare & args]
  `(loop [p# ~problem
          heap# (pm/priority-map-by ~compare)]
     (if-let [children# (seq (children p#))]
       (if-let [next-heap# (try (into heap# ~search-xf children#)
                                (catch TimeoutException _#))]
         (combine->recur p# next-heap# ~@args)
         p#)
       (if-let [result# (stop p#)]
         result#
         (combine->recur p# heap# ~@args)))))

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

(def depth-first-comparator (fn [a b] (compare b a)))
(def priority-xf (map (fn [x] [x (priority x)])))
;; create TimeoutException ahead of time and only once, since
;; creating an exception is expensive and we are not interested
;; in the stacktrace.
(def timeout-exception (new TimeoutException))

(defn timeout-xf [timeout control-chan]
  (let [timed-out? (volatile! false)]
    (async/go (async/<! (async/timeout timeout))
              (when control-chan (async/close! control-chan))
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
         parallel? (< 1 parallelism)
         control-chan (when parallel?
                        (async/chan (priority-queue chan-size compare root-problem)
                                    xf))
         search-xf (if timeout
                     (comp (timeout-xf timeout control-chan) (xform init) priority-xf)
                     (comp (xform init) priority-xf))
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
         (when control-chan (async/close! control-chan))))))
  ([init chan-size parallelism]
   (parallel-depth-first-search init {:chan-size   chan-size
                                      :parallelism parallelism})))
