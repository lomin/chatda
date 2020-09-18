(ns user
  (:require [me.lomin.chatda.matcher :refer [=*] :as matcher]
            [criterium.core :as c]
            [clojure.test :refer :all]
            [clojure.core.async :as async]
            [me.lomin.chatda.search :as search]
            [clojure.core.async.impl.protocols :as async-protocols]
            [com.rpl.specter :as s]
            [me.lomin.chatda.diff :as diff]
            [clj-async-profiler.core :as prof]
            [clojure.core.async :as async]))


(defn problem [a b]
  (matcher/equal-star-problem a b))

(defn ch
  ([p]
   (search/children (search/transduce-1 (search/xform p) p)))
  ([n ps]
   (sequence (search/xform (first ps)) (search/children (nth ps n)))))

(defn search-nodes
  ([xs]
   (sequence (mapcat ch)
             xs))
  ([a b]
   (->> (problem a b)
        (ch)
        (iterate search-nodes)
        (take-while seq)
        (sequence cat))))

(defn =' [a b & options]
  (as-> (matcher/equal-star-problem a b) $
        (apply search/parallel-depth-first-search $ (or (seq options)
                                                        '({})))))

(comment
  (let [a [[:x 1 :y :z] [:x :y] :c [[:x 1 :y :z] [:x :y] :c :d]]
        b [[:x :y :z] [:x 1 :y :z] :c [[:x :y :z] [:x 1 :y :z] :c :d]]]
    (c/quick-bench (=* a b)))

  (let [a [[:x 1 :y :z] [:x :y] :c [[:x 1 :y :z] [:x :y] :c :d]]
        b [[:x :y :z] [:x 1 :y :z] :c [[:x :y :z] [:x 1 :y :z] :c :d]]]
    (prof/profile (=* a b)))


  (let [a [[:x 1 :y :z] [:x :y] :c [[:x 1 :y :z] [:x :y] :c :d]]
        a' [[:x 1 :y :z] a [:x :y] :c [[:x 1 :y :z] [:x :y] :c :d a]]
        b [[:x :y :z] [:x 1 :y :z] :c [[:x :y :z] [:x 1 :y :z] :c :d]]
        b' [[:x :y :z] b [:x 1 :y :z] :c [[:x :y :z] [:x 1 :y :z] :c :d b]]
        ]
    (time (=* a' b')))
  )
