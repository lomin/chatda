(ns me.lomin.chatda.matcher-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.matcher :refer [=*] :as matcher]
            [com.rpl.specter :as s]
            [lambdaisland.deep-diff.diff :refer [->Mismatch ->Deletion ->Insertion] :as diff1]
            [arrangement.core :refer [rank]]
            [me.lomin.chatda.diff :as diff]))

(defn solve [problem]
  (let [{[left-source right-source] :source :as best-solution} (matcher/find-best problem (:best problem))]
    (vec (sort rank (map (fn [[left-path right-path]]
                           [(s/select-first (diff/path->selectors left-path) left-source)
                            (s/select-first (diff/path->selectors right-path) right-source)])
                         (:fails best-solution))))))

(deftest solve-test

  (is (= [] (-> (matcher/subset-problem #{1}
                                        #{1})
                (search/parallel-depth-first-search 2 2)
                (solve))))

  (is (= []
         (-> (matcher/subset-problem #{1}
                                     #{1 2})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[2 nil]]
         (-> (matcher/subset-problem #{1 2}
                                     #{1})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/subset-problem [1 2]
                                     [1 2])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/subset-problem [1 [2 3]]
                                     [1 [2 3]])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[nil 3]]
         (-> (matcher/subset-problem [1 2]
                                     [1 2 3])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[3 nil]]
         (-> (matcher/subset-problem [1 2 3]
                                     [1 2])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/subset-problem [1 #{2}]
                                     [1 #{2 3}])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[5 4]]
         (-> (matcher/subset-problem #{1 5 #{2}}
                                     #{1 4 #{2 3}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[3 nil]
          [5 4]]
         (-> (matcher/subset-problem #{1 5 #{2 3}}
                                     #{1 4 #{2}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/subset-problem #{1 #{2 {:a 1}}}
                                     #{1 4 #{2 3 {:a 1 :b 2}}})
             (search/parallel-depth-first-search 1 1)
             (solve))))

  (is (= [[{:a 1} 3]]
         (-> (matcher/subset-problem #{1 #{2 {:a 1}}}
                                     #{1 4 #{2 3}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[1 2]]
         (-> (matcher/subset-problem {{:a 1} 3}
                                     {{:a 2} 3})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[:b nil]]
         (-> (matcher/subset-problem {:a {:b 1}}
                                     {:a {}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[:e nil]]
         (-> (matcher/subset-problem {:a {:b 2 :c {:d 4 :e 5}}}
                                     {:a {:b 2 :c {:d 4}}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[1 5]
          [4 6]]
         (-> (matcher/subset-problem {#{1} #{2 3 4}}
                                     {#{5} #{2 3 6}})
             (search/parallel-depth-first-search 10 4)
             (solve))))

  (is (= [[1 0]
          [3 4]]
         (-> (matcher/subset-problem {#{1 #{2 3} 4} 5}
                                     {#{0 #{2 4} 4} 5})
             (search/parallel-depth-first-search 10 4)
             (solve)))))

(deftest path-to-diff
  (is (= [1 (->Mismatch 2 3)]
         (=* [1 2] [1 3])
         (diff1/diff [1 2] [1 3])))

  (is (= #{1 (->Deletion 2)}
         (=* #{1 2} #{1})
         (diff1/diff #{1 2} #{1})))

  (is (= {:a {(->Deletion :b) 1}}
         (=* {:a {:b 1}}
             {:a {}})
         (diff1/diff {:a {:b 1}}
                     {:a {}})))

  (is (= {{(->Deletion :b) 1
           :c              2} :a}
         (=* {{:b 1 :c 2} :a}
             {{:c 2} :a})))

  (is (= {(->Deletion {:b 1 :c 2}) :a
          (->Insertion {:c 2})     :a}
         (diff1/diff {{:b 1 :c 2} :a}
                     {{:c 2} :a})))

  (is (= [#{1} #{3}]
         (=* [#{1} #{3}]
             [#{1 2} #{3 4}])))

  (is (= [#{(->Deletion 2) 1}
          #{(->Deletion 4) 3}]
         (=* [#{1 2} #{3 4}]
             [#{1} #{3}])
         (diff1/diff [#{1 2} #{3 4}]
                     [#{1} #{3}])))

  (is (= {#{1} #{1 2}}
         (=* {#{1} #{1 2}}
             {#{1 2} #{1 2 3}})))

  (is (= {{#{1} #{3}} #{5}}
         (=* {{#{1} #{3}} #{5}}
             {{#{1 2} #{3 4}} #{5 6}})))

  (is (= {#{1 (->Deletion 2)} :a}
         (=* {#{1 2} :a}
             {#{1} :a})))

  (is (= {(->Deletion #{1 2}) :a
          (->Insertion #{1})  :a}
         (diff1/diff {#{1 2} :a}
                     {#{1} :a}))))

(comment

  (defn do-solve [a b & args]
    (-> (apply search/parallel-depth-first-search (matcher/subset-problem a b) (or (seq args) [1 1]))
        :best))

  (defn ch
    ([p]
     (search/children (search/transduce-1 (search/xform p) p)))
    ([n ps]
     (sequence (search/xform (first ps)) (search/children (nth ps n))))))
