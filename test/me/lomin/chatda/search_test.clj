(ns me.lomin.chatda.search-test
  (:require [clojure.test :refer :all]
            [clojure.data.priority-map :as pm]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.number-tree-test :as ntt]))

(deftest min-heap-test
  (is (= [:c [4 1]]
         (peek (pm/priority-map-by search/depth-first-comparator
                                   :a [3 2] :b [2 3] :c [4 1] :d [4 0]))))

  (is (= [[:a [3 2]] [:b [2 3]]]
         (sequence (remove (fn [[_ [a]]] (<= 4 a))
                           (pm/priority-map-by search/depth-first-comparator
                                               :a [3 2] :b [2 3] :c [4 1] :d [4 0]))))))

;; If the timeout timeout does not take effect, this test takes about
;; 1 second on my tiny machine.
(deftest timeout-test
  (testing "timeout takes effect before computation finishes"
    (is (> 1E300
           (-> (ntt/search-root 20 5E300)
               (search/parallel-depth-first-search {:parallelism 4
                                                    :chan-size   4
                                                    :timeout     10})
               :value
               #_time)))))