(ns me.lomin.chatda.a-star
  (:require [me.lomin.chatda.search :as search]))

(defn stack? [problem]
  (boolean (seq (:stack problem))))

(defn better? [p0 p1]
  (let [stack-0? (stack? p0)
        stack-1? (stack? p1)]
    (neg? (compare [stack-0? (:costs p0)]
                   [stack-1? (:costs p1)]))))

(defn choose-better [defender challenger]
  (if (better? challenger defender)
    challenger
    defender))

(defn calculate-complete-costs [problem]
  (+ (:costs problem)
     (transduce (map (or (:heuristic problem) (constantly 0)))
                +
                (:stack problem))))

(def NOOP-XFORM (map identity))

(defn make-xform [k problem]
  (if-let [xform (k problem)]
    (xform problem)
    NOOP-XFORM))

(defrecord AStarProblem [stack best-costs priority seen complete-costs children continue?]
  search/Searchable
  (children [this]
    (when-let [comparison (and (continue? this)
                               (< complete-costs @best-costs)
                               (peek stack))]
      (children comparison (update this :stack pop))))
  (xform [this]
    (comp
      (make-xform :xform this)
      (map #(let [$complete-costs (calculate-complete-costs %)]
              (-> %
                  (assoc :complete-costs $complete-costs)
                  (assoc :priority [$complete-costs (:depth %)]))))
      (remove #(<= @best-costs (:complete-costs %)))
      (map #(update % :depth (fnil dec 0)))))
  search/AsyncSearchable
  (xform-async [this]
    (comp
      (remove #(<= @best-costs (:complete-costs % 0)))
      (make-xform :xform-async this)))
  search/ExhaustiveSearch
  (stop [{:keys [diffs] :as this}]
    (when (not (stack? this))
      (swap! best-costs min (:costs this))
      (cond-> this
              (empty? diffs) (reduced))))
  search/Combinable
  (combine [_ other] other)
  (combine-async [this other] (choose-better this other))
  search/Prioritizable
  (priority [_] priority))

(defn a-star-problem [m]
  (map->AStarProblem (merge {:continue?      (constantly true)
                             :children       (constantly nil)
                             :compare        compare
                             :stack          (list)
                             :depth          0
                             :costs          0
                             :complete-costs 0
                             :best-costs     (atom Integer/MAX_VALUE)}
                            m)))