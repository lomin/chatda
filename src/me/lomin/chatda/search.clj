(ns me.lomin.chatda.search
  ^{:doc "Concurrency is the problem of scheduling multiple largely independent
tasks onto a usually smaller set of computational resources.
Parallelism on the other hand means breaking a computational task down into several
sub-tasks that can be processed independently and whose results are combined
afterwards, upon completion."}
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async-protocols]
            [me.lomin.chatda.threadpool :as thread-pool])
  (:import (java.util.concurrent TimeoutException TimeUnit ScheduledExecutorService)
           (clojure.lang IPersistentStack Counted IEditableCollection ITransientCollection IMeta IObj)
           (java.util PriorityQueue Comparator)))

#_(set! *warn-on-reflection* true)

(def ^:dynamic *force-parallel-search?* false)

(defprotocol Searchable
  ;; Must return nil, an empty sequence or a sequence with items
  ;;of type `Searchable`.
  (children [self])
  ;; Must return a transducer that every child and grandchild of
  ;; the root problem is transduced through.
  (xform [self])
  ;; Must return a number value representing the priority of a problem.
  (priority [self])
  ;; There are two different ways to stop a search:
  ;; 1. `stop` return an object wrapped in `reduced`:
  ;; This will stop all workers immediately and return the object
  ;; 2. `stop` returns a truthy value:
  ;; This will stop the worker that returned the truthy value, but
  ;; different workers will still continue their search.
  (stop [this children])
  (combine [this other]))

(defprotocol AsyncSearchable
  ;; Must return a transducer. With the exeption of the root problem,
  ;; every result of a worker that is of type `AsyncSearchable`, is
  ;; transduced through this transducer.
  (xform-async [self])
  (combine-async [this other]))

(deftype PriorityQueueBuffer
  [^long n ^PriorityQueue buf]
  async-protocols/Buffer
  (full? [_] (<= n (.size buf)))
  (remove! [_] (.poll buf))
  (add!* [this itm] (.offer buf itm) this)
  (close-buf! [_])
  Counted
  (count [_] (.size buf)))

(declare make-heap)
;; DO NOT USE Heap OUTSIDE THIS NAMESPACE!
;; It does not properly comply to the contract of the implemented
;; protocols in favor of performance optimization. This Heap implementation brings
;; a performance increase of about 30% compared to `clojure.data.priority-map`.
;; The use of Heap in this namespace is intended to look like idiomatic Clojure code,
;; but if it is used other than the fine-tuned accesses in this namespace, the
;; abstraction will probably break.
;; Heap behaves like a mutable variable with the API of a transient, but without
;; its semantics. For example, `(persistent! heap)` will return the same heap.
;; Neither Heap nor its underlying java.util.PriorityQueue are synchronized.
;; This is fine in this namespace, since we guarantee that a Heap has only
;; exactly one accessor, i.e. we guarantee thread isolation.
(deftype Heap
  [^Comparator compare ^PriorityQueue buf]
  Counted
  (count [_] (.size buf))
  IPersistentStack
  (peek [_] (when-let [item (.peek buf)] [item (priority item)]))
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
  (withMeta [self _] self))

(defn priority-comparator [compare]
  (fn [a b] (compare (priority a) (priority b))))

(defn priority-queue [^Comparator compare ^long n]
  (new PriorityQueueBuffer n
       (new PriorityQueue (max 1 n)
            ^Comparator (priority-comparator compare))))

(defn make-heap
  ([^Comparator compare]
   (new Heap
        compare
        (new PriorityQueue
             ^Comparator (priority-comparator compare)))))

(defn recur-sequential [_ first-problem next-heap]
  [[] `(recur ~first-problem ~next-heap)])

(defn recur-parallel [config first-problem next-heap]
  (let [chan (gensym)]
    [`[~chan (:control-chan ~config)]
     `(let [[second-problem#] (peek ~next-heap)
            offer# (when second-problem# (async/offer! ~chan second-problem#))]
        (cond
          ;; channel full or no problems left, so recur regularly
          (nil? offer#) (recur ~first-problem ~next-heap)
          ;; second-problem# was put onto chan, so forget about second-problem#
          (true? offer#) (recur ~first-problem (pop ~next-heap))
          ;; channel closed: stop immediately
          (false? offer#) ~first-problem))]))

(defmacro do-search [make-body start-problem config]
  (let [[config-sym search-xf compare next-problem next-heap] (repeatedly 5 gensym)
        [more-bindings body] ((resolve make-body) config-sym next-problem next-heap)
        bindings (into [{search-xf :search-xf compare :compare :as config-sym} config]
                       more-bindings)]
    `(let ~bindings
       (loop [problem# ~start-problem
              heap# (make-heap ~compare)]
         (let [children# (children problem#)]
           (if-let [result# (stop problem# children#)]
             result#
             (let [heap'# (try (into heap# ~search-xf children#)
                               (catch TimeoutException _#))
                   head-problem+priority# (peek heap'#)]
               (if head-problem+priority#
                 (let [~next-problem (combine problem# (first head-problem+priority#))
                       ~next-heap (pop heap'#)]
                   ~body)
                 problem#))))))))

(defn go-work [problem config]
  (async/go
    (try
      (do-search recur-parallel problem config)
      (catch Exception e e))))

(defn transduce-1
  "apply a transducer on a single value"
  [xform x]
  (first (into [] xform [x])))

(defn remove-worker-from [worker-pool worker]
  (filterv #(not= % worker) worker-pool))

(defn take-problem+chan-from
  [worker-pool control-chan parallelism]
  (if (<= parallelism (count worker-pool))
    (async/alts!! worker-pool)
    (if (seq worker-pool)
      (async/alts!! (conj worker-pool control-chan))
      [(async/poll! control-chan) control-chan])))

(defn search-parallel
  [{:keys [root-problem control-chan parallelism] :as config}]
  (loop [problem root-problem
         worker-pool [(go-work root-problem config)]]
    (let [[p ch]
          (take-problem+chan-from worker-pool control-chan parallelism)]
      (cond
        (nil? p) problem
        (reduced? p) @p
        (instance? Throwable p) (throw p)
        (= ch control-chan) (recur problem
                                   (conj worker-pool (go-work p config)))
        ;; => ch must be a go-worker and p is a new problem (or once the root-problem)
        :else (recur (if (identical? problem p) p (combine-async problem p))
                     (remove-worker-from worker-pool ch))))))

(defn search-sequential [{:keys [root-problem] :as config}]
  (let [result (do-search recur-sequential root-problem config)]
    (if (reduced? result)
      @result
      result)))

;; (compare a b) returns a comparator that favors the smaller element,
;; but if we want to favor an element deeper in the search space (further
;; from the root, i.e. with a higher depth value), we have to switch the arguments:
(def depth-first-comparator (fn [a b] (compare b a)))

;; Creating TimeoutException ahead of time and only once, since
;; creating an exception is expensive and we are not interested
;; in the stacktrace. This significantly reduces the latency
;; between the moment of the timeout and the unblocking of the
;; main thread.
(def timeout-exception (new TimeoutException))

(defn search-in-parallel? [parallelism]
  (or (< 1 parallelism) *force-parallel-search?*))

(defn check-config! [problem parallelism]
  (when (and (search-in-parallel? parallelism)
             (not (satisfies? AsyncSearchable problem)))
    (throw (new IllegalArgumentException
                ^Throwable
                (ex-info "Problems that do not implement AsyncSearchable
                  must be searched sequentially"
                         {:parallelism parallelism
                          :problem     problem})))))

(defn init! [config root-problem parallelism compare]
  @thread-pool/custom-thread-pool-executor
  (merge {:search-alg   search-sequential
          :root-problem root-problem
          :parallelism  parallelism
          :chan-size    1
          :search-xf    (xform root-problem)
          :compare      compare
          :timeout      nil
          :control-chan nil}
         config))

(defn init-async-config [{:keys [root-problem compare chan-size] :as config}]
  (let [xf (xform-async root-problem)]
    (-> config
        (assoc :search-alg search-parallel)
        (assoc :root-problem (transduce-1 xf root-problem))
        (assoc :control-chan (async/chan (priority-queue compare chan-size) xf)))))

(defn init-timeout-config! [{:keys [timeout control-chan] :as config}]
  (let [timed-out? (volatile! false)
        timeout-xf (map #(if @timed-out? (throw timeout-exception) %))]
    (-> config
        (assoc :timeout-future
               (.schedule ^ScheduledExecutorService @thread-pool/timeout-executor
                          ^Runnable #(do (when control-chan (async/close! control-chan))
                                         (vreset! timed-out? true))
                          ^long timeout
                          TimeUnit/MILLISECONDS))
        (update :search-xf #(comp timeout-xf %)))))

(defn search
  ([{:keys [compare] :or {compare depth-first-comparator} :as root-problem}
    {:keys [parallelism timeout] :as partial-config
     :or   {parallelism 1}}]
   (check-config! root-problem parallelism)
   (let [{search-with :search-alg :as config}
         (cond-> (init! partial-config root-problem parallelism compare)
                 (search-in-parallel? parallelism) init-async-config
                 timeout init-timeout-config!)]
     (try
       (search-with config)
       (finally
         (some-> config :timeout-future future-cancel)
         (some-> config :control-chan async/close!)))))
  ([init chan-size parallelism]
   (search init {:chan-size chan-size :parallelism parallelism})))
