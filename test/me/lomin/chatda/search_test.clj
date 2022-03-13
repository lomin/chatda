(ns me.lomin.chatda.search-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.number-tree-test :as ntt]))

(defn search-with-timeout [{:keys [timeout] :as options}]
  (let [start-time (. System (currentTimeMillis))
        result (-> (ntt/make-parallel-number-tree-search-config 20 5E300)
                   (merge options)
                   search/search
                   :value)
        end-time (. System (currentTimeMillis))
        duration (- end-time start-time)
        accepted-duration (* timeout 2)]
    [result duration accepted-duration]))

(deftest timeout-test
  (let [[_ duration accepted-duration]
        (search-with-timeout {:parallelism 100
                              :chan-size   100
                              :timeout     50})]
    (is (<= duration accepted-duration)))

  (let [[_ duration accepted-duration]
        (search-with-timeout {:parallelism 1
                              :chan-size   1
                              :timeout     50})]
    (is (<= duration accepted-duration))))