(ns me.lomin.chatda.search
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]))

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

(defprotocol DepthFirstSearchable
  (depth [self]))

(defn depth-first-comparator [depth-first-searchable-0
                              depth-first-searchable-1]
  (let [result (- (depth depth-first-searchable-0)
                  (depth depth-first-searchable-1))]
    (if (= result 0) -1 result)))

(defn stack-buffer [^long n]
  (new PriorityQueueBuffer n
       (new java.util.PriorityQueue n
            ^java.util.Comparator depth-first-comparator)))

(defprotocol ExhaustiveSearch
  (stop? [self]))

(extend-protocol ExhaustiveSearch
  Object (stop? [_] true)
  nil (stop? [_] true))

(defprotocol Combinable
  (combine [self other]))

(extend-protocol Combinable
  Object (combine [_ other] other)
  nil (combine [_ other] other))

(defmacro combine->offer->recur [$ stack chan]
  `(let [[$first# & [spawn# & $nnext# :as $next#]] ~stack]
     (if (async-protocols/closed? ~chan)
       (combine ~$ $first#)
       (recur
         (combine ~$ $first#)
         (if (and spawn# (async/offer! ~chan spawn#))
           $nnext#
           $next#)))))

(defn add-async-worker-to [worker-pool problem xform chan]
  (->> (async/go-loop [$ problem
                       stack (list)]
         (if-let [$children (seq (children $))]
           (if-let [next-stack (seq (into stack xform $children))]
             (combine->offer->recur $ next-stack chan)
             $)
           (cond
             (stop? $) (reduced $)
             (empty? stack) $
             :else (combine->offer->recur $ stack chan))))
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
  [root-problem xform [main-chan timeout-chan :as control-chans] parallelism]
  (loop [problem root-problem
         worker-pool []]
    (let [[$val ch] (next-channel-value worker-pool control-chans parallelism)]
      (cond
        (reduced? $val) @$val
        (= ch main-chan) (recur problem
                           (add-async-worker-to worker-pool
                                                $val xform main-chan))
        (= ch :default) problem
        (= ch timeout-chan) (do (async/close! main-chan)
                                problem)
        :else (recur (combine problem $val)
                     (remove-worker-from worker-pool ch))))))

(defn parallel-depth-first-search
  ([init {:keys [chan-size parallelism timeout] :or {chan-size   10
                                                     parallelism 4}}]
   (let [xform (xform init)
         root-problem (transduce-1 xform init)
         chan (async/chan (stack-buffer chan-size))
         control-chans (cond-> [chan]
                               timeout (conj (do (future (Thread/sleep timeout)
                                                         (async/close! chan))
                                                 (async/timeout timeout))))]
     (async/>!! chan root-problem)
     (let [result (rec:parallel-depth-first-search root-problem
                                                   xform
                                                   control-chans
                                                   parallelism)]
       (async/close! chan)
       result)))
  ([init chan-size parallelism]
   (parallel-depth-first-search init {:chan-size chan-size
                                      :parallelism parallelism})))
