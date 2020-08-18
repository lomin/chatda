(ns me.lomin.chatda.matcher
  (:require [clojure.test :as clojure-test]
            [clojure.data :as data]
            [kaocha.report]
            [kaocha.output :as output]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.diff :as diff]
            [com.rpl.specter :as s]))

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

(defn equality-partition-set [[a b] _]
  (hash-set (data/equality-partition a)
            (data/equality-partition b)))

(defmulti children equality-partition-set)

(defn heuristic-dispatch [problem]
  (equality-partition-set (peek (:stack problem)) problem))

(defmulti heuristic heuristic-dispatch)

(defn complete-costs [problem]
  (+ (:costs problem) (heuristic problem)))

(defn atom-count [x] (or (::count (meta x)) 1))
(defn atom-count-seq [x] (::count-seq (meta x)))

(defmethod heuristic :default [problem]
  (atom-count (first (peek (:stack problem)))))

(defmethod heuristic #{:atom} [problem]
  (let [[left right] (peek (:stack problem))]
    (if (= left right) 0 1)))

(defmethod heuristic #{:sequential} [problem]
  (let [[left-xs right-xs :as comparison] (peek (:stack problem))
        [a-xs b-xs] (if (<= (count left-xs) (count right-xs))
                      comparison
                      (reverse comparison))
        diff-count (- (count b-xs) (count a-xs))]
    (transduce (take diff-count) + (atom-count-seq b-xs))))

(defn set|map:heuristic [problem]
  (let [[left right] (peek (:stack problem))]
    (if (<= (count left) (count right))
      0
      (let [diff-count (- (count left) (count right))]
        (transduce (take diff-count) + (atom-count-seq left))))))

(defmethod heuristic #{:set} [problem]
  (set|map:heuristic problem))

(defmethod heuristic #{:map} [problem]
  (set|map:heuristic problem))

(defn add-diff [problem left]
  (-> problem
      (update :diffs conj [(:left-path problem) (:right-path problem)])
      (update :costs + (atom-count left))))

(defmethod children :default [[left] problem]
  (list (add-diff problem left)))

(defmethod children #{:atom} [[left right] problem]
  (list (cond-> problem
                (not= left right) (add-diff left))))

(defn path-tag
  ([tag elem] (path-tag tag elem elem))
  ([tag elem val] (if (= elem ::diff/nil) [::diff/nil val] [tag val])))

(defmacro stack-updates
  ([path-pair values]
   [[::pop] values [::push (mapv (partial cons 'path-tag) path-pair)]])
  ([path-0 values-0 _ pred path-1 values-1]
   `(if ~pred [[::pop] ~values-1 [::push (path-tag ~@path-1)]
               [::pop] ~values-0 [::push (path-tag ~@path-0)]]
              [[::pop] ~values-0 [::push (path-tag ~@path-0)]])))

(def seq:take-until-both-empty-xf
  (take-while (comp (partial not= [::diff/nil ::diff/nil])
                    (partial take 2))))

(def seq:mapcat-stack-updates-xf
  (mapcat (fn [[left right index]]
            (stack-updates [[:index left index] [:index right index]]
                           [left right]))))

(defmethod children #{:sequential}
  [[left-xs right-xs] problem]
  (list (update problem :stack into
                (comp seq:take-until-both-empty-xf
                      seq:mapcat-stack-updates-xf)
                (map vector
                     (concat left-xs (repeat ::diff/nil))
                     (concat right-xs (repeat ::diff/nil))
                     (range)))))

(defn set|map:ensure-same-length-as [xs compare-xs nil-value]
  (cond-> xs (< (count xs) (count compare-xs)) (conj nil-value)))

(defn set|map:children
  [set|map:stack-updates dis nil-value [left right] problem]
  (if (empty? left)
    (list problem)
    (let [l (first left)
          left-1 (dis left l)]
      (for [r (-> right (set|map:ensure-same-length-as left nil-value))]
        (cond-> problem
                (seq left-1) (update :stack conj [left-1 (dis right r)])
                :always (update :stack into (set|map:stack-updates l r)))))))

(def set:nil-value ::diff/nil)

(defn set:dis [s x] (disj s x))

(defn set:stack-updates [left right]
  (stack-updates [[:set left] [:set right]] [left right]))

(defmethod children #{:set}
  [comparison problem]
  (set|map:children set:stack-updates set:dis set:nil-value comparison problem))

(def map:nil-value [::diff/nil ::diff/nil])

(defn map:dis [m [k]] (dissoc m k))

(defn map:stack-updates [[left-key left-value] [right-key right-value]]
  (stack-updates [[:m-key left-key] [:m-key right-key]]
                 [left-key right-key]
                 :when (and (not= ::diff/nil left-key)
                            (not= ::diff/nil right-key))
                 [[:m-val left-key] [:m-val right-key]]
                 [left-value right-value]))

(defmethod children #{:map}
  [comparison problem]
  (set|map:children map:stack-updates map:dis map:nil-value comparison problem))

(defn stack? [problem]
  (boolean (seq (:stack problem))))

(defn better? [p0 p1]
  (neg? (compare (into [(stack? p0) (:depth p0)] (search/priority p0))
                 (into [(stack? p1) (:depth p1)] (search/priority p1)))))

(defn choose-better [defender challenger]
  (if (better? challenger defender)
    challenger
    defender))

(defn choose-best [& problems]
  (when-let [ps (seq (filter some? problems))]
    (-> (reduce choose-better ps)
        (dissoc :best))))

(defn push-path [problem [left-path right-path]]
  (-> problem
      (update :left-path conj left-path)
      (update :right-path conj right-path)
      (update :stack pop)))

(defn pop-path [problem]
  (-> problem
      (update :left-path pop)
      (update :right-path pop)
      (update :stack pop)))

(defn update-path [problem]
  (loop [{stack :stack :as p} problem]
    (if-let [[k v] (peek stack)]
      (condp = k
        ::push (recur (push-path p v))
        ::pop (recur (pop-path p))
        p)
      p)))

(defrecord EqualStarProblem []
  search/Searchable
  (children [{stack :stack :as problem}]
    (when-let [comparison (peek stack)]
      (map update-path
           (children comparison (update problem :stack pop)))))
  (xform [_]
    (comp (remove #(< @(:best-costs %) (complete-costs %)))
          (map #(update % :depth (fnil dec 0)))))
  search/ExhaustiveSearch
  (stop [{:keys [diffs best-costs] :as this}]
    (swap! best-costs min (:costs this))
    (cond-> this
            (empty? diffs) (reduced)))
  search/Combinable
  (combine [this other]
    (update other :best choose-best other this (:best this)))
  search/Prioritizable
  (priority [this] [(complete-costs this)
                    (hash (:diffs this))]))

(def meta-count-xf
  (map (comp (fnil identity 1) ::count meta)))

(defn add-count-meta [x]
  (let [xs (if (map? x) (mapcat seq x) (seq x))
        xf (cond-> meta-count-xf
                   (map? x) (comp (partition-all 2)
                                  (map (partial apply +))))]
    (as-> x $
          (->> xs
               (into '() xf)
               (sort)
               (vary-meta $ assoc ::count-seq))
          (->> xs
               (transduce meta-count-xf +)
               (inc)
               (vary-meta $ assoc ::count)))))

(def coll-walker+meta-nav
  (s/recursive-path
    [] p
    (s/if-path coll?
               (s/continue-then-stay
                 [s/ALL-WITH-META p]))))

(defn prepare [x]
  (s/transform coll-walker+meta-nav
               add-count-meta
               x))

(defn equal-star-problem [left right]
  (let [left* (prepare left)
        right* (prepare right)]
    (map->EqualStarProblem {:compare    compare
                            :source     [left* right*]
                            :stack      (list [left* right*])
                            :diffs      '()
                            :depth      0
                            :costs      0
                            :left-path  []
                            :right-path []
                            :best-costs (atom Integer/MAX_VALUE)})))

(defn =*
  ([a b] (=* a b nil))
  ([a b options]
   (as-> (equal-star-problem a b) $
         (search/parallel-depth-first-search $ options)
         (choose-best $ (:best $))
         (diff/diff (:diffs $) (:source $)))))