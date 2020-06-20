(ns me.lomin.chatda.search
 (:require
    [clojure.core.async :as async]
    [clojure.core.async.impl.protocols :as async-buffer]))

(set! *warn-on-reflection* true)

(defprotocol Searchable
  (children [self])
  (xform [self]))

(extend-type nil
  Searchable
  (children [_] nil))

(deftype OrderedSetBuffer [^long n ^java.util.TreeSet buf]
  async-buffer/Buffer
  (full? [this]
    (>= (.size buf) n))
  (remove! [this]
    (.pollLast buf))
  (add!* [this itm]
    (.add buf itm)
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
  (new OrderedSetBuffer n
       (new java.util.TreeSet
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
     (recur
       (combine ~$ $first#)
       (if (and spawn# (async/offer! ~chan spawn#))
         $nnext#
         $next#))))

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
  (let [f (xform combine)]
    (f nil x)))

(defn remove-worker-from [worker-pool worker]
  (filterv #(not= % worker) worker-pool))

(defn next-channel-value [worker-pool chan parallelism]
  (if (<= parallelism (count worker-pool))
    (async/alts!! worker-pool)
    (if (seq worker-pool)
      (async/alts!! (conj worker-pool chan))
      (async/alts!! [chan] :default nil))))

(defn rec:parallel-depth-first-search [root-problem xform chan parallelism]
  (loop [problem root-problem
         worker-pool []]
    (let [[$val ch] (next-channel-value worker-pool chan parallelism)]
      (cond
        (reduced? $val) @$val
        (= ch chan) (recur problem
                           (add-async-worker-to worker-pool $val xform chan))
        (= ch :default) problem
        :else (recur (combine problem $val)
                     (remove-worker-from worker-pool ch))))))

(defn parallel-depth-first-search
  ([init chan-size parallelism]
   (let [xform (xform init)
         root-problem (transduce-1 xform init)
         chan (async/chan (stack-buffer chan-size))]
     (async/>!! chan root-problem)
     (let [result (rec:parallel-depth-first-search root-problem
                                                   xform
                                                   chan
                                                   parallelism)]
       (async/close! chan)
       result))))
