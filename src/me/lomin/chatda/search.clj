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

(def IDENTITY-XFORM (map identity))

;; # Protocols

(defprotocol SearchableNode
  ;; Must return nil, an empty sequence or a sequence with items
  ;;of type SearchableNode.
  (children [self])
  ;; Must return a number value representing the priority of a SearchableNode.
  (priority [self])
  ;; There are two different ways to stop a search:
  ;; 1. `stop` return an object wrapped in `reduced`:
  ;; This will stop all workers immediately and return the object
  ;; 2. `stop` returns a truthy value:
  ;; This will stop the worker that returned the truthy value, but
  ;; different workers will still continue their search.
  (stop [this children])
  ;; Whenever a new SearchableNode is taken from the heap, it will be
  ;; combined with the previous top prioritized SearchableNode by calling
  ;; `(combine current previous)`.
  (combine [this previous]))

(defprotocol ParallelSearchableNode
  ;; When a search is done in parallel, multiple workers can return a
  ;;result. In order to reduce these multiple results into a single
  ;; result, `reduce-combine` takes the role of the reducing function.
  ;; No guarantees about `this` and `other` are given except that both
  ;; are of type ParallelSearchableNode.
  (reduce-combine [this other]))

;; # Comparators

(defn priority-comparator [compare-priority]
  (fn [a b] (compare-priority (priority a) (priority b))))

(def smaller-is-better compare)

(def larger-is-better (fn [a b] (compare b a)))

;; # PriorityQueueBuffer

(deftype PriorityQueueBuffer
  [^long n ^PriorityQueue buf]
  async-protocols/Buffer
  (full? [_] (<= n (.size buf)))
  (remove! [_] (.poll buf))
  (add!* [this itm] (.offer buf itm) this)
  (close-buf! [_])
  Counted
  (count [_] (.size buf)))

(defn make-priority-queue-buffer [^Comparator compare-priority ^long n]
  (new PriorityQueueBuffer n
       (new PriorityQueue (max 1 n)
            ^Comparator (priority-comparator compare-priority))))

;; # Heap

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
  [^Comparator compare-priority ^PriorityQueue buf]
  Counted
  (count [_] (.size buf))
  IPersistentStack
  (peek [_] (when-let [item (.peek buf)] [item (priority item)]))
  (pop [self] (.poll buf) self)
  (cons [self item] (.offer buf item) self)
  (empty [_] (make-heap compare-priority))
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

(defn make-heap
  ([^Comparator compare-priority]
   (new Heap
        compare-priority
        (new PriorityQueue
             ^Comparator (priority-comparator compare-priority)))))

;; # Main search algorithm for both sequential and parallel search.

(defmacro do-search [make-body start-node config]
  (let [[config-sym search-xf compare-priority next-node next-heap] (repeatedly 5 gensym)
        [more-bindings body] ((resolve make-body) config-sym next-node next-heap)
        bindings (into [{search-xf :search-xf compare-priority :compare-priority
                         :as       config-sym} config]
                       more-bindings)]
    `(let ~bindings
       (loop [node# ~start-node
              heap# (make-heap ~compare-priority)]
         (let [children# (children node#)]
           (if-let [result# (stop node# children#)]
             result#
             (let [heap'# (try (into heap# ~search-xf children#)
                               (catch TimeoutException _#))
                   head-node+priority# (peek heap'#)]
               (if head-node+priority#
                 (let [~next-node (combine (first head-node+priority#) node#)
                       ~next-heap (pop heap'#)]
                   ~body)
                 node#))))))))

;; # Sequential Search

(defn recur-sequential [_ first-node next-heap]
  [[] `(recur ~first-node ~next-heap)])

(defn search-sequential [{:keys [root-node] :as config}]
  (let [result (do-search recur-sequential root-node config)]
    (if (reduced? result)
      @result
      result)))

;; # Parallel Search

(defn recur-parallel [config first-node next-heap]
  (let [chan (gensym)]
    [`[~chan (::control-chan ~config)]
     `(let [[second-node#] (peek ~next-heap)
            offer# (when second-node# (async/offer! ~chan second-node#))]
        (cond
          ;; channel full or no nodes left, so recur regularly
          (nil? offer#) (recur ~first-node ~next-heap)
          ;; second-node# was put onto chan, so forget about second-node#
          (true? offer#) (recur ~first-node (pop ~next-heap))
          ;; channel closed: stop immediately
          (false? offer#) ~first-node))]))

(defn go-work [node config]
  (async/go
    (try
      (do-search recur-parallel node config)
      (catch Exception e e))))

(defn remove-worker-from [worker-pool worker]
  (filterv #(not= % worker) worker-pool))

(defn take-node+chan-from
  [worker-pool control-chan parallelism]
  (if (<= parallelism (count worker-pool))
    (async/alts!! worker-pool)
    (if (seq worker-pool)
      (async/alts!! (conj worker-pool control-chan))
      [(async/poll! control-chan) control-chan])))

(defn search-parallel
  [{:keys [root-node parallelism] control-chan ::control-chan :as config}]
  (loop [current-best-node root-node
         worker-pool [(go-work root-node config)]]
    (let [[node ch]
          (take-node+chan-from worker-pool control-chan parallelism)]
      (cond
        (nil? node) current-best-node
        (reduced? node) @node
        (instance? Throwable node) (throw node)
        (= ch control-chan) (recur current-best-node
                                   (conj worker-pool (go-work node config)))
        ;; => ch must be a go-worker and node is a new node (or once the root-node)
        :else (recur (if (identical? node current-best-node)
                       node
                       (reduce-combine node current-best-node))
                     (remove-worker-from worker-pool ch))))))

;; # Configuration

(defn search-in-parallel? [config]
  (or (<= 2 (get config :parallelism 1))
      *force-parallel-search?*))

(defn check-config! [{root-node :root-node :as config}]
  (when (and (search-in-parallel? config)
             (not (satisfies? ParallelSearchableNode root-node)))
    (throw (new IllegalArgumentException
                ^Throwable
                (ex-info "Only a ParallelSearchableNode can be searched in parallel"
                         {:parallelism (:parallelism config)
                          :node        root-node})))))

(defn init! [default-config config]
  @thread-pool/custom-thread-pool-executor
  (merge default-config config))

(defn init-async-config [{:keys [compare-priority chan-size search-xf-async] :as config}]
  (-> config
      (assoc ::search-alg search-parallel)
      (assoc ::control-chan (async/chan (make-priority-queue-buffer compare-priority chan-size)
                                        search-xf-async))))

;; Creating TimeoutException ahead of time and only once, since
;; creating an exception is expensive and we are not interested
;; in the stacktrace. This significantly reduces the latency
;; between the moment of the timeout and the unblocking of the
;; main thread.
(def timeout-exception (new TimeoutException))

(defn init-timeout-config! [{timeout :timeout control-chan ::control-chan :as config}]
  (let [timed-out? (volatile! false)
        timeout-xf (map #(if @timed-out? (throw timeout-exception) %))]
    (-> config
        (assoc ::timeout-future
               (.schedule ^ScheduledExecutorService @thread-pool/timeout-executor
                          ^Runnable #(do (when control-chan (async/close! control-chan))
                                         (vreset! timed-out? true))
                          ^long timeout
                          TimeUnit/MILLISECONDS))
        (update :search-xf #(comp timeout-xf %)))))

(def DEFAULT-CONFIG
  {:root-node        nil
   :parallelism      1
   :chan-size        1
   :search-xf        IDENTITY-XFORM
   :search-xf-async  IDENTITY-XFORM
   :compare-priority larger-is-better
   :timeout          nil
   ;; internal
   ::search-alg      search-sequential
   ::control-chan    nil
   ::timeout-future  nil})

;; # API

(defn search [{timeout :timeout :as search-config}]
  (check-config! search-config)
  (let [{search-with ::search-alg :as complete-config}
        (cond-> (init! DEFAULT-CONFIG search-config)
                (search-in-parallel? search-config) init-async-config
                timeout init-timeout-config!)]
    (try
      (search-with complete-config)
      (finally
        (some-> complete-config ::timeout-future future-cancel)
        (some-> complete-config ::control-chan async/close!)))))
