(ns user
  (:require [vijual :as v]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.a-star :as a-star]
            [me.lomin.chatda.a-star-test :as a-star-test]
            [me.lomin.chatda.number-tree-test :as ntt]
            [me.lomin.chatda.n-queens-test :as n-queens]
            [clj-async-profiler.core :as prof]
            [criterium.core :as c]))

(defn tree [n
            {:keys [depth view xf]
             :or   {depth 3
                    view  identity
                    xf    (map identity)}
             :as   opts}]
  (when (pos? (inc depth))
    (into [(view n)]
          (filter some?)
          (mapv #(tree % (update opts :depth (fnil dec depth)))
                (sequence xf (search/children n))))))

(defn draw
  ([node] (v/draw-tree [(tree node {})]))
  ([node opts] (v/draw-tree [(tree node opts)])))

(def _0_partial-sum
  (comment

    ;; draw search tree
    (let [{:keys [root-node search-xf]}
          (ntt/make-parallel-number-tree-search-config 3 50)]
      (draw root-node {:view :value :xf search-xf :depth 3}))

    ;; bench sequential
    (c/quick-bench (ntt/search-number-tree-parallel :partial-sum 200000 1 1 3))

    ;; bench parallel
    (c/quick-bench (ntt/search-number-tree-parallel :partial-sum 200000 2 1 3))

    ;; profile
    (prof/profile {:return-file true}
                  (time (ntt/search-number-tree-parallel :value 200000 2 1 10)))))


(def _1_n-queens
  (comment

    ;; draw search tree
    (draw (:root-node (n-queens/all-n-queens-search 3))
          {:view :board :depth 3})

    ;; bench reference
    (c/quick-bench (n-queens/reference-n-queen 9))

    ;; bench sequential
    (c/quick-bench (n-queens/search-n-queens 9 1 1))

    ;; bench parallel
    (c/quick-bench (n-queens/search-n-queens 9 2 1))

    ))

(def _2_cities-of-europe
  (comment

    (let [{:keys [root-node search-xf]}
          (a-star-test/city-travel-search-config :Stockholm :Paris)]
      (draw root-node {:view  #(a-star-test/get-city-name (:graph root-node) (:current %))
                       :xf search-xf
                       :depth 2})
      (a-star-test/shortest-travel :Stockholm :Paris))))

