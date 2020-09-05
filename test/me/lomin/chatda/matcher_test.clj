(ns me.lomin.chatda.matcher-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.matcher :refer [=*] :as matcher]
            [com.rpl.specter :as s]
            [lambdaisland.deep-diff.diff :refer [->Mismatch ->Deletion ->Insertion left-undiff right-undiff] :as diff1]
            [arrangement.core :refer [rank]]
            [me.lomin.chatda.diff :as diff]
            [clojure.test.check.clojure-test :as test]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn diff-paths [problem]
  (:diffs (matcher/choose-best problem (:best problem))))

(defn solve [problem]
  (let [{[left-source right-source] :source :as best-solution}
        (matcher/choose-best problem (:best problem))]
    (vec (sort rank (map (fn [[left-path right-path]]
                           [(s/select-first (diff/path->navigators diff/navs left-path) left-source)
                            (s/select-first (diff/path->navigators diff/navs right-path) right-source)])
                         (:diffs best-solution))))))

(deftest solve-test

  (is (= [] (-> (matcher/equal-star-problem #{1}
                                            #{1})
                (search/parallel-depth-first-search 2 2)
                (solve))))

  (is (= []
         (-> (matcher/equal-star-problem #{1}
                                         #{1 2})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[2 ::diff/nil]]
         (-> (matcher/equal-star-problem #{1 2}
                                         #{1})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/equal-star-problem [1 2]
                                         [1 2])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[[[:index 2 :before]] [[:index 2]]]
          [[[:index 1 :before]] [[:index 1]]]]
         (-> (matcher/equal-star-problem [1]
                                         [1 2 3])
             (search/parallel-depth-first-search 2 2)
             (diff-paths))))

  (is (= (list [[[:index 0]] [[:index 0]]])
         (-> (matcher/equal-star-problem [nil 1] [0 1])
             (search/parallel-depth-first-search 2 2)
             (diff-paths))))

  (is (= (list [[[:set 2]]
                [[:set :me.lomin.chatda.diff/nil]]])
         (-> (matcher/equal-star-problem #{1 2} #{1})
             (search/parallel-depth-first-search 2 2)
             (diff-paths))))

  (is (= '()
         (-> (matcher/equal-star-problem #{1} #{1 2})
             (search/parallel-depth-first-search 2 2)
             (diff-paths))))

  (is (= '()
         (-> (matcher/equal-star-problem [1 [2 3]]
                                         [1 [2 3]])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[nil 3]]
         (-> (matcher/equal-star-problem [1 2]
                                         [1 2 3])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[3 ::diff/nil]]
         (-> (matcher/equal-star-problem [1 2 3]
                                         [1 2])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/equal-star-problem [1 #{2}]
                                         [1 #{2 3}])
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[5 4]]
         (-> (matcher/equal-star-problem #{1 5 #{2}}
                                         #{1 4 #{2 3}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[3 ::diff/nil]
          [5 4]]
         (-> (matcher/equal-star-problem #{1 5 #{2 3}}
                                         #{1 4 #{2}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= []
         (-> (matcher/equal-star-problem #{1 #{2 {:a 1}}}
                                         #{1 4 #{2 3 {:a 1 :b 2}}})
             (search/parallel-depth-first-search 1 1)
             (solve))))

  (is (= [[{:a 1} 3]]
         (-> (matcher/equal-star-problem #{1 #{2 {:a 1}}}
                                         #{1 4 #{2 3}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[1 2]]
         (-> (matcher/equal-star-problem {{:a 1} 3}
                                         {{:a 2} 3})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[:b ::diff/nil]]
         (-> (matcher/equal-star-problem {:a {:b 1}}
                                         {:a {}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[:e ::diff/nil]]
         (-> (matcher/equal-star-problem {:a {:b 2 :c {:d 4 :e 5}}}
                                         {:a {:b 2 :c {:d 4}}})
             (search/parallel-depth-first-search 2 2)
             (solve))))

  (is (= [[1 5]
          [4 6]]
         (-> (matcher/equal-star-problem {#{1} #{2 3 4}}
                                         {#{5} #{2 3 6}})
             (search/parallel-depth-first-search 10 4)
             (solve))))

  (is (= [[1 0]
          [3 4]]
         (-> (matcher/equal-star-problem {#{1 #{2 3} 4} 5}
                                         {#{0 #{2 4} 4} 5})
             (search/parallel-depth-first-search 10 4)
             (solve)))))

(deftest path-to-diff
  (is (= [1 (->Mismatch 2 3)]
         (=* [1 2] [1 3])
         (diff1/diff [1 2] [1 3])))

  (is (= [1 (->Insertion 2) (->Insertion 3)]
         (=* [1] [1 2 3])
         (diff1/diff [1] [1 2 3])))

  (is (= [(->Mismatch nil 0) 1]
         (=* [nil 1] [0 1])
         (diff1/diff [nil 1] [0 1])))

  (is (= [(->Deletion -1)]
         (=* [-1] '())
         (diff1/diff [-1] '())))

  (is (= #{1 (->Deletion 2)}
         (=* #{1 2} #{1})
         (diff1/diff #{1 2} #{1})))

  (is (= [1 (->Insertion 2)]
         (=* [1] [1 2])
         (diff1/diff [1] [1 2])))

  (is (= #{1 (->Mismatch 2 3)}
         (=* #{1 2} #{1 3})
         (diff1/diff #{1 2} #{1 3})))

  (is (= #{1 2}
         (=* #{1 2} #{1 2})
         (diff1/diff #{1 2} #{1 2})))

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
                     {#{1} :a})))

  (is (= #{(->Deletion "")
           (->Deletion 0)}
         (=* #{"" 0} #{})
         (=* #{"" 0} #{} {:chan-size   1
                          :parallelism 1
                          :timeout     1000})))

  (is (= {0 (->Mismatch 0 1)}
         (=* {0 0}
             {0 1, -1 0}
             {:chan-size   1
              :parallelism 1})
         (=* {0 0}
             {0 1, -1 0}
             {:chan-size   1
              :parallelism 1
              :timeout     1000})))

  (let [a [[:x 1 :y :z] [:x :y] :c :d]
        b [[:x :y :z] [:x 1 :y :z] :c :d]]
    (is (= [(->Insertion [:x :y :z])
            [:x 1 :y :z]
            (->Deletion [:x :y])
            :c :d]
           (diff1/diff a b)))
    (is (= [[:x (->Deletion 1) :y :z]
            [:x (->Insertion 1) :y (->Insertion :z)]
            :c
            :d]
           (=* a b))))

  (is (= [(->Deletion :b)
          :a
          (->Mismatch :x :y)]
         (=* [:b :a :x]
             [:a :y])))

  (is (= [(->Insertion :b)
          :a
          (->Mismatch :y :x)]
         (=* [:a :y]
             [:b :a :x]))))

(deftest prepare-test
  (is (= 22
         (matcher/atom-count
           (matcher/prepare {1 [4
                                5
                                {{1 2 3 4} {5 6 7 8}}
                                #{4 5 [6 7]}]}))))
  (is (= '(1 1 6 11)
         (matcher/atom-count-seq
           (matcher/prepare [4
                             5
                             {{1 2 3 4} {5 6 7 8}}
                             #{4 5 [6 7]}])))))

(deftest heuristic-test
  (is (= 0 (matcher/heuristic
             (matcher/equal-star-problem 1 1))))
  (is (= 1 (matcher/heuristic
             (matcher/equal-star-problem 1 2))))
  (is (= 1 (matcher/heuristic
             (matcher/equal-star-problem #{} {}))))
  (is (= 4 (matcher/heuristic
             (matcher/equal-star-problem '(1)
                                         (range 5)))))
  (is (= 6 (matcher/heuristic
             (matcher/equal-star-problem '((1) (1 2 3))
                                         '()))))
  (is (= 0 (matcher/heuristic
             (matcher/equal-star-problem #{1}
                                         #{3 4}))))
  (is (= 0 (matcher/heuristic
             (matcher/equal-star-problem #{1 2}
                                         #{3 4}))))
  (is (= 1 (matcher/heuristic
             (matcher/equal-star-problem #{1 2}
                                         #{3}))))
  (is (= 4 (matcher/heuristic
             (matcher/equal-star-problem #{(range 5) (range 3)}
                                         #{3}))))
  (is (= 0 (matcher/heuristic
             (matcher/equal-star-problem {1 2}
                                         {3 4 5 6}))))
  (is (= 2 (matcher/heuristic
             (matcher/equal-star-problem {3 4 5 6}
                                         {1 2}))))

  (is (= 7
         (matcher/heuristic
           (matcher/equal-star-problem {(range 5) 2 3 (range 10)}
                                       {1 2})))))

(def containers (fn [inner-gen]
                  (gen/one-of [(gen/list inner-gen)
                               (gen/vector inner-gen)
                               (gen/set inner-gen)
                               (gen/map inner-gen inner-gen)])))

(def scalars (gen/frequency [[10 gen/simple-type-printable-equatable]
                             [1 (gen/return nil)]]))

(defn insertion? [x]
  (instance? lambdaisland.deep_diff.diff.Insertion x))

(def failure-parallel nil)
(def end-2-end-generative-parallel-test nil)
(test/defspec end-2-end-generative-parallel-test
  {:num-tests 100
   ;:seed 1599306615414
   }
  (prop/for-all [chan-size (gen/fmap inc gen/nat)
                 parallelism (gen/fmap inc gen/nat)
                 left (gen/recursive-gen containers scalars)
                 right (gen/recursive-gen containers scalars)]
    (let [d (=* left right {:chan-size   chan-size
                            :parallelism parallelism
                            :timeout     100})]
      (or (= d :timeout)
          (= d left)
          (and (or (insertion? d) (= left (left-undiff d))))
          (and (def failure-parallel [left right d {:chan-size   chan-size
                                                    :parallelism parallelism
                                                    :timeout     100}])
               false)))))

(def timeout-failure nil)
(def timeout-test-test nil)
(test/defspec timeout-test-test
  {:num-tests 100}
  (prop/for-all [chan-size (gen/no-shrink (gen/fmap inc gen/nat))
                 parallelism (gen/no-shrink (gen/fmap inc gen/nat))
                 timeout (gen/no-shrink (gen/choose 10 500))
                 left (gen/no-shrink (gen/recursive-gen containers scalars))
                 right (gen/no-shrink (gen/recursive-gen containers scalars))]
    (let [start-time (. System (currentTimeMillis))
          d (=* left right {:chan-size   chan-size
                            :parallelism parallelism
                            :timeout     timeout})
          end-time (. System (currentTimeMillis))
          duration (- end-time start-time)
          accepted-duration (* timeout 1.5)]
      (or (< duration accepted-duration)
          (and (def timeout-failure [duration accepted-duration timeout d])
               false)))))