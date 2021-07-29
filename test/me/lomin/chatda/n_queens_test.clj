(ns me.lomin.chatda.n-queens-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.search :as search]))

(defmacro todo> [[f & params :as f-call] & clauses]
  (if (resolve f)
    (cons 'do
          (into (list f-call)
                (for [[example-params expected] (partition 2 clauses)]
                  (list 'is (list '= expected (cons f example-params))))))
    (cons 'condp (cons '= (cons (vec params) clauses)))))

(defn safe?
  "Check if queen in last row is safe"
  [positions]
  (let [[queen-pos & left] positions
        k (count positions)
        diags-up (map - left (range 1 (inc k)))
        diags-down (map + left (range 1 (inc k)))]
    (empty? (filter (partial = queen-pos) (concat diags-up diags-down left)))))

;; source: https://gist.github.com/juergenhoetzel/633108
(defn queens [board-size]
  (into #{}
        ((fn queen-cols [k]
           (if (zero? k)
             (list ())
             (for [rest-of-queens (queen-cols (dec k))
                   new-row (range 1 (inc board-size))
                   :let [new-cols (cons new-row rest-of-queens)]
                   :when (safe? new-cols)]
               new-cols)))
         board-size)))

(defn same-column? [[_ y0] [_ y1]]
  (= y0 y1))

(defn abs [number]
  (if (pos? number)
    number
    (- number)))

(defn same-diagonal?
  "Returns true if the given queens are exactly diagonal from each other."
  [q1 q2]
  (= (abs (- (first q1) (first q2)))
     (abs (- (second q1) (second q2)))))

(defn queens-in-check2?
  ([latest-queen-coords other-queen-coords]
   (or (same-column? latest-queen-coords other-queen-coords)
       (same-diagonal? latest-queen-coords other-queen-coords)))
  ([latest-queen-coords other-queen-x other-queen-y]
   (queens-in-check2? latest-queen-coords [other-queen-x other-queen-y])))

(defn format-board [board]
  (reduce-kv (fn [board* k v]
               (if (neg? (dec k))
                 (reduced nil)
                 (assoc board* (dec k) v)))
             (vec (repeat (count board) nil))
             board))

(defn consistent?
  ([node]
   (let [board (:board node)
         current (:current node)]
     (consistent? board [current (board current)])))
  ([board [row :as latest-queen-coordinates]]
   (not-any? (partial queens-in-check2? latest-queen-coordinates)
             (seq (dissoc board row)))))

(deftest consistent?-test
  (are [expected board latest-queen-coordinates]
    (= expected (consistent? board latest-queen-coordinates))
    true {} [1 1]
    true {1 1} [2 3]
    true {1 3} [2 1]
    true {1 1} [1 1]
    false {1 1} [2 1]
    false {1 1} [2 2]
    false {3 3} [1 1]))

(defn find-mrv [vars]
  (first (reduce-kv (fn [[_ lowest-vars :as low] row vars*]
                      (let [vars-count (count vars*)]
                        (if (< vars-count
                               (or lowest-vars Integer/MAX_VALUE))
                          [row vars-count]
                          low)))
                    nil
                    vars)))

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

(defn cut-vars [vars [x _ :as latest-queen-coords]]
  (let [init (dissoc vars x)]
    (reduce-kv
      (fn [vars' row open-columns]
        (let [columns (into #{}
                            (remove (partial queens-in-check2?
                                             latest-queen-coords
                                             row))
                            open-columns)]
          (if (seq columns)
            (assoc vars' row columns)
            (dissoc vars' row))))
      init
      init)))

(deftest cut-vars-test
  (are [expected vars latest-queen-coords]
    (= expected (cut-vars vars latest-queen-coords))
    {} {1 #{1 2} 2 #{1 2}} [1 1]
    {} {1 #{1 2} 2 #{1 2}} [1 2]
    {} {1 #{1 2} 2 #{1 2}} [2 1]
    {} {1 #{1 2} 2 #{1 2}} [2 2]
    {1 #{2} 2 #{3}} {1 #{1 2 3} 2 #{1 2 3} 3 #{1 2 3}} [3 1]))

(defrecord AllNQueensSearch [n board current vars]
  search/SearchableNode
  (children [this] (let [mrv (find-mrv vars)
                         candidate-vars (vars mrv)]
                     (map #(-> this
                               (assoc :current mrv)
                               (update :board assoc mrv %)
                               (update :vars (fn [vars*] (cut-vars vars* [mrv %]))))
                          candidate-vars)))
  (priority [this] 0)
  (stop [this children])
  (combine [this other]
    (cond-> this
            (seq (:solutions other)) (update :solutions into (:solutions other))
            (= n (count board)) (update :solutions conj (format-board board))))
  search/ParallelSearchableNode
  (reduce-combine [this other] (search/combine this other)))

(defn make-vars [n]
  (into {} (for [i (range 1 (inc n))]
             [i (set (range 1 (inc n)))])))

(defn all-n-queens-search [n]
  {:root-node (map->AllNQueensSearch {:n         n
                                      :board     {}
                                      :current   0
                                      :solutions #{}
                                      :vars      (make-vars n)})
   :search-xf (filter consistent?)})

(defn children [config]
  (mapv (partial merge {})
        (search/children (:root-node config))))

(deftest children-test
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
         (children (all-n-queens-search 3)))))

(defn search-n-queens
  ([n] (search-n-queens n nil))
  ([n opts]
   (:solutions (search/search (merge (all-n-queens-search n)
                                     opts)))))