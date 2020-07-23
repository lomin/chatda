(ns me.lomin.chatda.diff-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.diff :as diff]))

(deftest grow-path-tree-test
  (is (= [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
         (diff/grow-path-tree diff/root-node
                              [[[:m-val #{3}]] [[:m-val #{1 2}]]])))

  (is (= [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]
                          [::diff/node [:m-key #{3}] [[::diff/node [:set 3] [[::diff/leaf [[:set 1]]]]]]]]]
         (diff/grow-path-tree [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
                              [[[:m-key #{3}] [:set 3]] [[:set 1]]])))

  (is (= [::diff/node []
          [[::diff/node [:m-val #{3}]
            [[::diff/leaf [[:m-val #{1 2}]]]]]
           [::diff/node [:m-key #{3}]
            [[::diff/node [:set 3]
              [[::diff/node [:test :me]
                [[::diff/leaf [[:set 1]]]]]]]]]]]
         (diff/grow-path-tree [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
                              [[[:m-key #{3}] [:set 3] [:test :me]] [[:set 1]]])))

  (is (= [::diff/node []
          [[::diff/node [::diff/nil]
            [[::diff/leaf [[:index 1]]]]]]]
         (diff/grow-path-tree diff/root-node
                              [[[::diff/nil]] [[:index 1]]])))

  (is (= [::diff/node []
          [[::diff/node [::diff/nil]
            [[::diff/leaf [[:index 1]]]
             [::diff/leaf [[:index 2]]]]]]]
         (diff/grow-path-tree [::diff/node []
                               [[::diff/node [::diff/nil]
                                 [[::diff/leaf [[:index 1]]]]]]]
                              [[[::diff/nil]] [[:index 2]]]))))