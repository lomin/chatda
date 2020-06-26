(ns me.lomin.chatda.diff-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.matcher :as matcher]
            [me.lomin.chatda.diff :as diff]))

(deftest sort-paths-test

  (is (= [:index :m-val :set :m-key ::diff/nil nil]
         (sort diff/compare-tags [:m-key nil ::diff/nil :set :index :m-val])))

  (is (= [:m-key :m-val]
         (diff/first-different-tags
           [[[:m-key #{3}] [:set 3]] [[:set 1]]]
           [[[:m-val #{3}]] [[:m-val #{1 2}]]])))

  (is (= [:set nil]
         (diff/first-different-tags
           [[[:m-key #{3}] [:set 3]] [[:set 1]]]
           [[[:m-key #{3}]] [[:m-val #{1 2}]]])))

  (is (= [:set :index]
         (diff/first-different-tags
           [[[:m-key #{3}] [:set 3]] [[:set 1]]]
           [[[:m-key #{3}] [:index 3]] [[:m-val #{1 2}]]])))

  (is (= nil
         (diff/first-different-tags
           [[[:m-key #{3}] [:index 3]] [[:set 1]]]
           [[[:m-key #{3}] [:index 3]] [[:m-val #{1 2}]]])))

  (is (= [:m-key nil]
         (diff/first-different-tags
           [[[:m-key #{3}] [:set 3]] [[:set 1]]]
           [[] [[:m-val #{1 2}]]])))

  (is (= [[[[:m-val #{3}]] [[:m-val #{1 2}]]]
          [[[:m-key #{3}] [:set 3]] [[:set 1]]]]
         (sort diff/compare-paths
               #{[[[:m-key #{3}] [:set 3]] [[:set 1]]]
                 [[[:m-val #{3}]] [[:m-val #{1 2}]]]})))

  (is (= [[[[:m-val #{3}] [:set 4]] [[:m-val #{1 2}] [:set 3]]]
          [[[:m-key #{3}] [:set 3]] [[:m-key #{1 2}] [:set 1]]]]
         (sort diff/compare-paths
               #{[[[:m-key #{3}] [:set 3]] [[:m-key #{1 2}] [:set 1]]]
                 [[[:m-val #{3}] [:set 4]] [[:m-val #{1 2}] [:set 3]]]}))))

(deftest paths->tree-test
  (is (= [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
         (diff/path->selector-tree [::diff/node [] []]
                                   [[[:m-val #{3}]] [[:m-val #{1 2}]]])))

  (is (= [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]
                             [::diff/node [:m-key #{3}] [[::diff/node [:set 3] [[::diff/leaf [[:set 1]]]]]]]]]
         (diff/path->selector-tree [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
                                   [[[:m-key #{3}] [:set 3]] [[:set 1]]])))

  (is (= [::diff/node []
          [[::diff/node [:m-val #{3}]
            [[::diff/leaf [[:m-val #{1 2}]]]]]
           [::diff/node [:m-key #{3}]
            [[::diff/node [:set 3]
              [[::diff/node [:test :me]
                [[::diff/leaf [[:set 1]]]]]]]]]]]
         (diff/path->selector-tree [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
                                   [[[:m-key #{3}] [:set 3] [:test :me]] [[:set 1]]]))))