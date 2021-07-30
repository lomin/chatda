(ns me.lomin.chatda.n-queens-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.search :as search]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as check]
            [clojure.test.check.generators :as gen]))

;; source: https://gist.github.com/juergenhoetzel/633108
(defn reference-n-queen [board-size & _]
  (letfn [(safe? [positions]
            (let [[queen-pos & left] positions
                  k (count positions)
                  diags-up (map - left (range 1 (inc k)))
                  diags-down (map + left (range 1 (inc k)))]
              (empty? (filter (partial = queen-pos) (concat diags-up diags-down left)))))]
    (into #{}
          ((fn queen-cols [k]
             (if (zero? k)
               (list ())
               (for [rest-of-queens (queen-cols (dec k))
                     new-row (range 1 (inc board-size))
                     :let [new-cols (cons new-row rest-of-queens)]
                     :when (safe? new-cols)]
                 new-cols)))
           board-size))))

(defn abs [number]
  (if (pos? number) number (- number)))

(defn same-column? [[_ y0] [_ y1]]
  (= y0 y1))

(defn same-diagonal? [[x0 y0] [x1 y1]]
  (= (abs (- x0 x1)) (abs (- y0 y1))))

(defn queens-in-check?
  ([latest-queen-coords other-queen-coords]
   (or (same-column? latest-queen-coords other-queen-coords)
       (same-diagonal? latest-queen-coords other-queen-coords)))
  ([latest-queen-coords other-queen-x other-queen-y]
   (queens-in-check? latest-queen-coords [other-queen-x other-queen-y])))

(defn format-board [board]
  (reduce-kv (fn [board* k v] (assoc board* (dec k) v))
             (vec (repeat (count board) nil))
             board))

(defn find-mrv [vars]
  (first
    (reduce-kv
      (fn [[_ lowest-vars-count :as lowest-row+vars-count] row vars*]
        (let [vars-count (count vars*)]
          (cond
            (= vars-count 1) (reduced [row vars-count])
            (< vars-count
               (or lowest-vars-count Integer/MAX_VALUE)) [row vars-count]
            :else lowest-row+vars-count)))
      nil
      vars)))

(defn cut-vars [vars [x :as latest-queen-coords]]
  (let [init (dissoc vars x)]
    (reduce-kv
      (fn [vars' row open-columns]
        (let [columns (into #{}
                            (remove (partial queens-in-check?
                                             latest-queen-coords
                                             row))
                            open-columns)]
          (if (seq columns)
            (assoc vars' row columns)
            (dissoc vars' row))))
      init
      init)))

(defn solution? [n board]
  (= n (count board)))

;; using Backtracking + MRV heuristic (Minimum Remaining Values)
(defrecord AllNQueensSearch [n board current vars]
  search/SearchableNode
  (children [this] (let [mrv (find-mrv vars)]
                     (map #(-> this
                               (assoc :current mrv)
                               (update :board assoc mrv %)
                               (update :vars (fn [vars*] (cut-vars vars* [mrv %]))))
                          (vars mrv))))
  (priority [this] (count board))
  (stop [this children])
  (combine [this other]
    (cond-> this
            (seq (:solutions other)) (assoc :solutions (:solutions other))
            (solution? n board) (update :solutions conj (format-board board))))
  search/ParallelSearchableNode
  (reduce-combine [this {other-board :board :as other}]
    (cond-> (update this :solutions into (:solutions other))
            (solution? n board) (update :solutions conj (format-board board))
            (solution? n other-board) (update :solutions conj (format-board other-board)))))

(defn make-vars [n]
  (into {} (for [i (range 1 (inc n))]
             [i (set (range 1 (inc n)))])))

(defn all-n-queens-search [n]
  {:root-node (map->AllNQueensSearch {:n         n
                                      :board     {}
                                      :current   0
                                      :solutions #{}
                                      :vars      (make-vars n)})})

(defn search-n-queens
  ([n]
   (search-n-queens n nil))
  ([n opts]
   (:solutions (search/search (merge (all-n-queens-search n) opts))))
  ([n parallelism chan-size]
   (search-n-queens n {:parallelism parallelism
                       :chan-size   chan-size})))

(deftest find-mrv-test
  (are [expected vars]
    (= expected (find-mrv vars))
    1 {1 #{1 2 3}
       2 #{1 2 3}
       3 #{1 2 3}}
    2 {1 #{1 2 3}
       2 #{1 3}
       3 #{1 2 3}}
    nil {}))

(deftest cut-vars-test
  (are [expected vars latest-queen-coords]
    (= expected (cut-vars vars latest-queen-coords))
    {} {1 #{1 2} 2 #{1 2}} [1 1]
    {} {1 #{1 2} 2 #{1 2}} [1 2]
    {} {1 #{1 2} 2 #{1 2}} [2 1]
    {} {1 #{1 2} 2 #{1 2}} [2 2]
    {1 #{2} 2 #{3}} {1 #{1 2 3} 2 #{1 2 3} 3 #{1 2 3}} [3 1]))

(deftest children-test
  (letfn [(children [config]
            (mapv (partial merge {})
                  (search/children (:root-node config))))]
    (is (= [{:board     {1 1}
             :current   1
             :n         2
             :solutions #{}
             :vars      {}}
            {:board     {1 2}
             :current   1
             :n         2
             :solutions #{}
             :vars      {}}]
           (children (all-n-queens-search 2))))
    (is (= [{:board     {1 1}
             :current   1
             :n         3
             :solutions #{}
             :vars      {2 #{3}
                         3 #{2}}}
            {:board     {1 3}
             :current   1
             :n         3
             :solutions #{}
             :vars      {2 #{1}
                         3 #{2}}}
            {:board     {1 2}
             :current   1
             :n         3
             :solutions #{}
             :vars      {3 #{1 3}}}]
           (children (all-n-queens-search 3))))))

(defmacro make-reporter-fn [make-expected make-actual]
  `(fn [m#]
     (when (= :shrunk (:type m#))
       (let [input# (-> m# :shrunk :smallest)]
         (is (= (apply ~make-expected input#)
                (apply ~make-actual input#)))))))

(check/defspec n-queens-test
  {:num-tests   20
   :max-size    10
   :reporter-fn (make-reporter-fn reference-n-queen
                                  search-n-queens)}
  (prop/for-all [n (gen/fmap inc gen/nat)
                 parallelism (gen/fmap inc gen/nat)
                 chan-size (gen/fmap inc gen/nat)]
    (= (reference-n-queen n)
       (search-n-queens n parallelism chan-size))))