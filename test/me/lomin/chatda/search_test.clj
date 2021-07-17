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

(defn search-with-timeout [{:keys [timeout] :as config}]
  (let [start-time (. System (currentTimeMillis))
        result (-> (ntt/search-root 20 5E300)
                   (search/parallel-depth-first-search config)
                   :value)
        end-time (. System (currentTimeMillis))
        duration (- end-time start-time)
        accepted-duration (* timeout 2)]
    [result duration accepted-duration]))

;; If the timeout timeout does not take effect, this test takes about
;; 1 second on my tiny machine.
(deftest timeout-test
  (let [[result duration accepted-duration]
        (search-with-timeout {:parallelism 100
                              :chan-size   100
                              :timeout     50})]
    (is (> 1E200 result))
    (is (<= duration accepted-duration)))

  (let [[result duration accepted-duration]
        (search-with-timeout {:parallelism 1
                              :chan-size   1
                              :timeout     50})]
    (is (> 1E200 result))
    (is (<= duration accepted-duration))))