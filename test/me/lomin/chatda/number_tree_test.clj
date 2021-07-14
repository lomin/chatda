(ns me.lomin.chatda.number-tree-test
  (:refer-clojure :exclude [ancestors])
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.map-coloring-test :as sequential]
            [me.lomin.chatda.search :as p]
            [me.lomin.chatda.search :as search]))

(defn cnt:parent [{:keys [value branch] :as problem}]
  (when (pos? value)
    (assoc problem :value (bigint (/ (dec value) branch)))))

(defn cnt:ancestors [problem]
  (take-while some?
              (rest (iterate cnt:parent problem))))

(defn +=cnt:count [self other]
  (update other :count + (:count self)))

(defrecord CountNumberTreeSearch []
  search/Searchable
  (children [{:keys [value branch max-size] :as problem}]
    (map (partial assoc problem :value)
         (range (inc (* branch value))
                (inc (min (* branch (inc value))
                          max-size)))))
  (xform [_]
    (map #(assoc % :count (:value %))))
  (priority [this] (count (cnt:ancestors this)))
  (stop [_ _] false)
  p/Combinable
  (combine [this other]
    (+=cnt:count this other))
  (combine-async [this other]
    (p/combine this other)))

(defn cnt:count-root [branch-factor max-size]
  (map->CountNumberTreeSearch {:value    0
                               :branch   branch-factor
                               :max-size max-size}))

(defn cnt:child-value [value branch-factor child-index]
  (+' (*' branch-factor value) child-index))

(defrecord ParallelNumberTreeSearch []
  search/Searchable
  (children [{:keys [value branch max-size] :as problem}]
    (into (list)
          (comp (map (fn [i]
                       (assoc problem
                         :value
                         (cnt:child-value value branch i))))
                (filter (fn [{v :value}] (<= v max-size))))
          (range 1 (inc branch))))
  (xform [_] (filter (comp even? :value)))
  (priority [this] (count (cnt:ancestors this)))
  (stop [this children] (when (empty? children) (reduced this))))

(defn search-root [branch-factor max-size]
  (map->ParallelNumberTreeSearch {:value    0
                                  :branch   branch-factor
                                  :max-size max-size}))

(deftest sequential-depth-first-search-test
  (let [branch-factor 21
        max-value 100000000
        _ (prn "sequential")
        result (time (sequential/sequential-backtracking (search-root branch-factor max-value)))]
    (prn (:value result))
    (is (empty? (search/children result)))
    (is (even? (:value result)))
    (is (<= (:value result) max-value))))

(deftest parallel-depth-first-search-test
  (let [branch-factor 21
        max-value 100000000
        _ (prn "parallel")
        result (time (p/parallel-depth-first-search (search-root branch-factor
                                                                 max-value)
                                                    5
                                                    5))]
    (prn (:value result))
    (is (empty? (search/children result)))
    (is (even? (:value result)))
    (is (<= (:value result) max-value))))

(defn sequential-partial-sum [problem]
  (->> (sequential/sequential-backtracking-seq (search/xform problem)
                                               (search/children problem))
       (take-while some?)
       (map :value)
       (reduce + 0)))

(deftest count-numbers-test

  (is (= 105 (time (sequential-partial-sum (cnt:count-root 5 14)))))

  (is (= 10 (-> (cnt:count-root 1 4)
                (p/parallel-depth-first-search 5 5)
                :count)))
  (is (= 10 (-> (cnt:count-root 2 4)
                (p/parallel-depth-first-search 5 5)
                :count)))
  (is (= 10 (-> (cnt:count-root 3 4)
                (p/parallel-depth-first-search 5 5)
                :count)))

  (is (= 105 (-> (cnt:count-root 5 14)
                 (p/parallel-depth-first-search 20 10)
                 :count
                 (time)))))


(comment
  (let [csp (cnt:count-root 11 1000000)]
    (send p/max-worker-size (constantly 0))
    (prn "sequential")
    ; (prn (time (sequential-sum csp)))
    (prn "parallel")
    (prn (-> csp
             (p/parallel-depth-first-search 1 4)
             :count
             (time))
         @p/max-worker-size)))