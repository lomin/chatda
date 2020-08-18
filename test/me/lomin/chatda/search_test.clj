(ns me.lomin.chatda.search-test
  (:require [clojure.test :refer :all]
            [clojure.data.priority-map :as pm]
            [me.lomin.chatda.search :as search]))

(deftest min-heap-test
  (is (= [:c [4 1]]
         (peek (pm/priority-map-by search/depth-first-comparator
                                   :a [3 2] :b [2 3] :c [4 1] :d [4 0])))))