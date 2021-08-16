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
  (when (pos? depth)
    (into [(view n)]
          (filter some?)
          (mapv #(tree % (update opts :depth (fnil dec depth)))
                (sequence xf (search/children n))))))

(defn draw
  ([node] (v/draw-tree [(tree node {})]))
  ([node opts] (v/draw-tree [(tree node opts)])))

(comment

  (draw (:root-node (n-queens/all-n-queens-search 5))
        {:view  :board
         :depth 4})

  (let [{:keys [root-node search-xf]}
        (ntt/make-parallel-number-tree-search-config 10 50)]
    (draw root-node {:view  :value :xf search-xf :depth 3}))

  (prof/profile {:return-file true}
                (time (ntt/search-number-tree-parallel :value 200000 2 1 10)))

  ;; sequential
  (do (require '[me.lomin.chatda.number-tree-test :as ntt])
      (c/quick-bench (ntt/search-number-tree-parallel :value 200000 1 1 10)))

  ;; parallel
  (do (require '[me.lomin.chatda.number-tree-test :as ntt])
      (c/quick-bench (ntt/search-number-tree-parallel :value 200000 2 1 10)))

  )

