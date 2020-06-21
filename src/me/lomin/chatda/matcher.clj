(ns me.lomin.chatda.matcher
  (:require [clojure.test :as clojure-test]
            [clojure.data :as data]
            [lambdaisland.deep-diff.diff :refer [->Mismatch
                                                 ->Deletion
                                                 ->Insertion]]
            [com.rpl.specter :as s]
            [kaocha.report]
            [kaocha.output :as output]
            [me.lomin.chatda.search :as search]))

(defmethod clojure-test/assert-expr '=* [msg form]
  (let [[_ expected actual] form]
    (let [args (rest form)
          pred (first form)]
      `(let [values# (list ~@args)
             result# (~pred ~expected ~actual)]
         (if (= ~expected result#)
           (clojure-test/do-report {:type     :pass, :message ~msg,
                                    :expected '~form, :actual (cons ~pred values#)})
           (clojure-test/do-report {:type     :fail, :message ~msg,
                                    :expected '~form, :actual (list '~'not (list '=* ~expected result#))}))
         result#))))

(defmethod kaocha.report/print-expr '=* [m]
  (let [printer (output/printer)]
    (let [[_ expected & actuals] (-> m :actual second)]
      (output/print-doc
        [:span
         "Expected:" :line
         [:nest (output/format-doc expected printer)]
         :break
         "Actual:" :line
         (into [:nest]
               (interpose :break)
               (for [actual actuals]
                 (output/format-doc actual
                                    printer)))]))))

(defprotocol Path
  (path [self x]))

(defn equality-partition-set [[a b] & _]
  (hash-set (data/equality-partition a) (data/equality-partition b)))

(defmulti children equality-partition-set)

(defn path-tag
  ([tag elem]
   (path-tag tag elem elem))
  ([tag elem val]
   (if (= elem ::nil) [::nil ::nil] [tag val])))

(extend-type Object Path
  (path [_ _] []))

(extend-type nil Path
  (path [_ _] []))

(extend-type java.util.Set Path
  (path [_ [a b]]
    [[::push [[:set a] [:set b]]] [a b] [::pop]]))

(extend-type java.util.Map Path
  (path [_ [a b]]
    (let [path [[::push [(path-tag :m-key (first a))
                         (path-tag :m-key (first b))]]
                [(first a) (first b)]
                [::pop]]]
      (if (and (not= ::nil (first a)) (not= ::nil (first b)))
        (into [[::push [(path-tag :m-val (first a))
                        (path-tag :m-val (first b))]]
               [(second a) (second b)]
               [::pop]]
              path)
        path))))

(extend-type clojure.lang.MapEntry Path
  (path [_ [a b]]
    [[a b]]))

(extend-type java.util.List Path
  (path [_ [a b index]]
    [[::push [(if (= a ::nil) [::nil ::nil] [:index index])
              (if (= b ::nil) [::nil ::nil] [:index index])]]
     [a b]
     [::pop]]))

(defmethod children :default [_ problem]
  (list (-> problem
            (update :fails conj [(:left-path problem)
                                 (:right-path problem)]))))

(defmethod children #{:atom} [[a b] problem]
  (list (-> (if (= a b)
              problem
              (update problem :fails conj [(:left-path problem)
                                           (:right-path problem)])))))

(defn col->path-xf [[col]]
  (mapcat (comp reverse (partial path col))))

(defmethod children #{:set}
  [[left-set right-set :as comparison] problem]
  (if (empty? left-set)
    (list problem)
    (for [left left-set
          right (if (<= (count left-set) (count right-set))
                  right-set
                  (conj right-set ::nil))]
      (let [left-set* (disj left-set left)]
        (as-> problem $
              (if (seq left-set*)
                (update $ :stack conj [(disj left-set left)
                                       (disj right-set right)])
                $)
              (update $ :stack into (col->path-xf comparison)
                      [[left right]]))))))

(defmethod children #{:map}
  [[left-map right-map :as comparison] problem]
  (if (empty? left-map)
    (list problem)
    (for [left left-map
          right (if (<= (count left-map) (count right-map))
                  right-map
                  (conj right-map [::nil ::nil]))]
      (-> problem
          (update :stack conj [(dissoc left-map (first left))
                               (dissoc right-map (first right))])
          (update :stack into (col->path-xf comparison) [[left right]])))))

(def take-until-both-empty-xf
  (take-while (comp (partial not= [::nil ::nil])
                    (partial take 2))))

(defmethod children #{:sequential}
  [[left-xs right-xs :as comparison] problem]
  (list (update problem
                :stack
                into
                (comp take-until-both-empty-xf
                      (col->path-xf comparison))
                (map vector
                     (concat left-xs (repeat ::nil))
                     (concat right-xs (repeat ::nil))
                     (range)))))

(defn success? [problem]
  (and (empty? (:stack problem))
       (empty? (:fails problem))))

(defn find-best [best & problems]
  (-> (if (success? best)
        best
        (reduce (fn [best* candidate]
                  (cond
                    (not candidate) best*
                    (success? candidate) (reduced candidate)
                    (< (search/depth best*)
                       (search/depth candidate)) candidate
                    :else best*))
                best
                problems))
      (dissoc :best)))

(defn safe-pop [xs] (if (seq xs) (pop xs) xs))

(defn push-path [problem [left-path right-path]]
  (-> problem
      (update :left-path conj left-path)
      (update :right-path conj right-path)
      (update :stack safe-pop)))

(defn pop-path [problem]
  (-> problem
      (update :left-path safe-pop)
      (update :right-path safe-pop)
      (update :stack safe-pop)))

(defrecord SubsetCompareProblem []
  search/Searchable
  (children [problem]
    (loop [{stack :stack :as p} problem]
      (when-let [[k v :as kv] (peek stack)]
        (condp = k
          ::push (recur (push-path p v))
          ::pop (recur (pop-path p))
          (children kv (update p :stack pop))))))
  (xform [_]
    (map (fn [p] (update p :depth (fnil inc 0)))))
  search/ExhaustiveSearch
  (stop? [this] (success? this))
  search/Combinable
  (combine [self other]
    (assoc other
      :best
      (find-best self
                 other
                 (:best self)
                 (:best other))))
  search/DepthFirstSearchable
  (depth [this] (- (:depth this 0)
                   (count (:fails this)))))

(defn subset-problem [left right]
  (map->SubsetCompareProblem {:source     [left right]
                              :stack      (list [left right])
                              :fails      #{}
                              :depth      0
                              :left-path  []
                              :right-path []}))

(def selector-mapping {::nil  (constantly s/AFTER-ELEM)
                       :set   #(s/set-elem %)
                       :m-key #(s/map-key %)
                       :m-val #(s/keypath %)
                       :index #(s/nthpath %)})

(defn path->selectors [path]
  (mapv (fn [[tag v]] ((selector-mapping tag) v)) path))

(def sort-mapping {nil    0
                   ::nil  1
                   :m-key 2
                   :set   3
                   :m-val 4
                   :index 5})

(defn compare-tags [l r]
  (- (sort-mapping r) (sort-mapping l)))

(defn extract-tags [path]
  (s/select [s/FIRST s/ALL s/FIRST] path))

(defn tag-seq [path]
  (concat (extract-tags path) (repeat nil)))

(defn first-different-tags [left-path right-path]
  (first (sequence (comp (take-while (fn [[l r]] (not= nil l r)))
                         (remove (fn [[l r]] (= l r))))
                   (map vector
                        (tag-seq left-path)
                        (tag-seq right-path)))))

(defn compare-paths [left-path right-path]
  (let [[l r] (first-different-tags left-path right-path)]
    (compare-tags l r)))

(defn diff [{:keys [source fails]}]
  (first
    (reduce (fn [[left-source right-source] [left-path right-path]]
              (let [right (s/select-first (path->selectors right-path)
                                          right-source)]
                [(s/transform (path->selectors left-path)
                              (fn [left]
                                (cond
                                  (or (nil? left)
                                      (= left :com.rpl.specter.impl/NONE)) (->Insertion right)
                                  (or (nil? right)
                                      (= right :com.rpl.specter.impl/NONE)) (->Deletion left)
                                  :else (->Mismatch left right)))
                              left-source)
                 right-source]))
            source
            (sort compare-paths fails))))

(defn =* [a b & args]
  (as-> (subset-problem a b) $
        (apply search/parallel-depth-first-search $ (or (seq args) [10 4]))
        (find-best $ (:best $))
        (diff $)))
