(ns me.lomin.chatda.map-coloring-test
  (:require [clojure.test :refer :all]
            [me.lomin.chatda.search :as search]))

(defn sequential-backtracking-seq [xform csps]
  (lazy-seq (if-let [[$first & $rest]
                     (sequence xform csps)]
              (cons $first
                    (sequential-backtracking-seq xform
                                                 (concat (when $first (search/children $first))
                                                         $rest))))))

(defn sequential-backtracking [csp]
  (first (filter (comp empty? search/children)
                 (sequential-backtracking-seq (search/xform csp)
                                              (search/children csp)))))

(defn constraint [state-a state-b]
  (fn [csp]
    (if-let [a (get-in csp [:assignment state-a])]
      (= a (get-in csp [:assignment state-b]))
      false)))

(defn init [domains variables constraints]
  {:domains                   domains
   :variables                 variables
   :any-constraints-violated? (apply some-fn constraints)
   :assignment                (zipmap variables (repeat nil))})


(defn select-unassigned-variable [csp]
  (first (filter (comp nil?
                       (partial get-in csp)
                       (partial conj [:assignment]))
                 (:variables csp))))

(defn assign-next-var [csp d]
  (if-let [$unassigned-variable (select-unassigned-variable csp)]
    (assoc-in csp
              [:assignment $unassigned-variable]
              d)
    nil))

(defn next-csps [csp]
  (filter some?
          (map (partial assign-next-var csp)
               (:domains csp))))


(defn complete? [csp]
  (and csp (every? some? (vals (:assignment csp)))))

(defn consistent? [{:keys [any-constraints-violated?]
                    :as   csp}]
  (not (any-constraints-violated? csp)))

(defrecord MapColoringCsp []
  search/Searchable
  (children [this] (next-csps this))
  (xform [_] (filter consistent?)))

(defrecord ParallelMapColoringCsp []
  search/Searchable
  (children [this] (next-csps this))
  (xform [_] (filter consistent?))
  (priority [this] (count (remove nil? (vals (:assignment this)))))
  (stop [this children] (when (empty? children) (reduced this)))
  (combine [_ other] other)
  search/AsyncSearchable
  (xform-async [self] (search/xform self))
  (combine-async [this other] (search/combine this other)))

(def csp (map->ParallelMapColoringCsp
           (init [:red :green :blue]
                 [:WA :NT :Q :NSW :V :SA :T]
                 #{(constraint :WA :NT)
                   (constraint :WA :SA)
                   (constraint :NT :SA)
                   (constraint :NT :Q)
                   (constraint :SA :Q)
                   (constraint :SA :NSW)
                   (constraint :SA :V)
                   (constraint :Q :NSW)
                   (constraint :V :T)})))

(defn complete-consistent? [$]
  (and (complete? $)
       (consistent? $)))

(deftest lazy-seq-csp-test
  (prn "lazy")
  (is (= true
         (-> csp
             (sequential-backtracking)
             (time)
             (complete-consistent?)))))


(deftest parallel-csp-test
  (prn "parallel")
  (is (= true
         (-> csp
             (map->ParallelMapColoringCsp)
             (search/parallel-depth-first-search 2 2)
             (time)
             (complete-consistent?)))))
