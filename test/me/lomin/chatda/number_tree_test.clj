(ns me.lomin.chatda.number-tree-test
  (:refer-clojure :exclude [ancestors])
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :as check]
            [me.lomin.chatda.search :as search]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn partial-sum [n]
  (/ (* n (inc n)) 2))

(defn abs [number] (if (pos? number) number (* -1 number)))

(defn child-value [value branch-factor child-index]
  (+' (*' branch-factor value) child-index))

(defrecord ParallelNumberTreeSearch [value branch max-size]
  search/Searchable
  (children [problem]
    (map (fn [i]
           (assoc problem
             :value
             (child-value value branch i)))
         (range 1 (inc branch))))
  (xform [_] (comp (filter #(<= (:value %) max-size))
                   (map #(assoc % :partial-sum (:value %)))))
  (priority [this] (abs (- (:value this) max-size)))
  (stop [this _] (when (<= max-size value) this))
  (combine [self other]
    (update other :partial-sum + (:partial-sum self)))
  search/AsyncSearchable
  (xform-async [_] (map #(assoc % :partial-sum (:value %))))
  (combine-async [this other]
    (if (< (search/priority this) (search/priority other))
      (search/combine other this)
      (search/combine this other))))

(defn make-parallel-number-tree-search-problem [branch-factor max-size]
  (map->ParallelNumberTreeSearch {:value       0
                                  :partial-sum 0
                                  :branch      branch-factor
                                  :max-size    max-size}))

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
    (= select :value) n
    (= select :partial-sum) (partial-sum n)))

(check/defspec number-tree-test
  {:num-tests   100
   :max-size    10000
   :reporter-fn (make-reporter-fn number-tree-properties
                                  search-number-tree-parallel)}
  (prop/for-all [select (gen/elements #{:value :partial-sum})
                 n (gen/fmap inc gen/nat)
                 parallelism (gen/fmap inc gen/nat)
                 chan-size (gen/fmap inc gen/nat)
                 branch-factor (gen/fmap inc gen/nat)]
    (= (number-tree-properties select n)
       (search-number-tree-parallel select n parallelism chan-size branch-factor))))

(comment
  (require '[clj-async-profiler.core :as prof])
  (prn (time (search-number-tree-parallel :value 2000000 2 1 2))))