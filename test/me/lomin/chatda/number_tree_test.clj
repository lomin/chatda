(ns me.lomin.chatda.number-tree-test
  (:refer-clojure :exclude [ancestors])
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :as check]
            [me.lomin.chatda.search :as search]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn partial-sum [n]
  (/ (* n (inc n)) 2))

(defn child-value [value branch-factor child-index]
  (+' (*' branch-factor value) child-index))

(defrecord ParallelNumberTreeSearch [value branch max-size]
  search/Searchable
  (children [problem]
    (when (< value max-size)
      (map (fn [i]
             (assoc problem
               :value
               (child-value value branch i)))
           (range 1 (inc branch)))))
  (xform [_] (comp (filter #(<= (:value %) max-size))
                   (map #(assoc % :partial-sum (:value %)))
                   (map #(assoc % :max-value (:value %)))))
  (priority [_] value)
  (stop [_ _])
  (combine [this other]
    (-> other
        (update :partial-sum + (:partial-sum this))
        (update :max-value max (:max-value this))))
  search/AsyncSearchable
  (xform-async [_] (filter #(<= (:value %) max-size)))
  (combine-async [this other] (search/combine this other)))

(defn make-parallel-number-tree-search-problem [branch-factor max-size]
  (map->ParallelNumberTreeSearch {:value            0
                                  :max-value        0
                                  :partial-sum      0
                                  :compare-priority search/smaller-priority-is-better
                                  :branch           branch-factor
                                  :max-size         max-size}))

(defmacro make-reporter-fn [make-expected make-actual]
  `(fn [m#]
     (when (= :shrunk (:type m#))
       (let [input# (-> m# :shrunk :smallest)]
         (is (= (apply ~make-expected input#)
                (apply ~make-actual input#)))))))

(defn search-number-tree-parallel
  ([n parallelism chan-size branch-factor]
   (search-number-tree-parallel identity n parallelism chan-size branch-factor))
  ([select n parallelism chan-size branch-factor]
   (-> (make-parallel-number-tree-search-problem branch-factor n)
       (search/search {:parallelism parallelism
                       :chan-size   chan-size})
       select)))

(defn number-tree-properties [select n & _]
  (cond
    (= select :max-value) n
    (= select :partial-sum) (partial-sum n)))

(deftest example-tests
  (is (= 5
         (number-tree-properties :max-value 5 1 1 2)
         (search-number-tree-parallel :max-value 5 1 1 2)))

  (is (= 11
         (number-tree-properties :max-value 11 1 1 2)
         (search-number-tree-parallel :max-value 11 1 1 2)))

  (is (= 15
         (number-tree-properties :partial-sum 5 1 1 2)
         (search-number-tree-parallel :partial-sum 5 1 1 2)))

  (is (= 66
         (number-tree-properties :partial-sum 11 1 1 2)
         (search-number-tree-parallel :partial-sum 11 1 1 2))))

(check/defspec number-tree-test
  {:num-tests   100
   :max-size    10000
   :reporter-fn (make-reporter-fn number-tree-properties
                                  search-number-tree-parallel)}
  (prop/for-all [select (gen/elements #{:max-value :partial-sum})
                 n (gen/fmap inc gen/nat)
                 parallelism (gen/fmap inc gen/nat)
                 chan-size (gen/fmap inc gen/nat)
                 branch-factor (gen/fmap inc gen/nat)]
    (= (number-tree-properties select n)
       (search-number-tree-parallel select n parallelism chan-size branch-factor))))

(comment
  (require '[clj-async-profiler.core :as prof])
  (prn (time (search-number-tree-parallel :max-value 2000000 2 1 2))))