(ns me.lomin.chatda.matcher
  (:require [clojure.test :as clojure-test]
            [clojure.data :as data]
            [kaocha.report]
            [kaocha.output :as output]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.diff :as diff]))

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

(defn path-tag
  ([tag elem] (path-tag tag elem elem))
  ([tag elem val] (if (= elem ::diff/nil) [::diff/nil val] [tag val])))

(defmacro stack-updates
  ([path-pair values]
   [[::pop] values [::push (mapv (partial cons 'path-tag) path-pair)]])
  ([path-0 value-0 _ pred path-1 value-1]
   `(if ~pred [[::pop] ~value-1 [::push (path-tag ~@path-1)]
               [::pop] ~value-0 [::push (path-tag ~@path-0)]]
              [[::pop] ~value-0 [::push (path-tag ~@path-0)]])))

(defn equality-partition-set [[a b] & _]
  (hash-set (data/equality-partition a)
            (data/equality-partition b)))

(defmulti children equality-partition-set)

(defmethod children :default [_ problem]
  (list (update problem :diffs conj [(:left-path problem)
                                     (:right-path problem)])))

(defmethod children #{:atom} [[a b] problem]
  (list (cond-> problem
                (not= a b) (update :diffs conj [(:left-path problem)
                                                (:right-path problem)]))))

(defn set|map:ensure-same-length-as [xs compare-xs nil-value]
  (cond-> xs (< (count xs) (count compare-xs)) (conj nil-value)))

(defn set|map:children
  [set|map:stack-updates dis nil-value [left right] problem]
  (if (empty? left)
    (list problem)
    (for [l left
          r (-> right (set|map:ensure-same-length-as left nil-value))
          :let [left-1 (dis left l)]]
      (cond-> problem
              (seq left-1) (update :stack conj [left-1 (dis right r)])
              :always (update :stack into (set|map:stack-updates l r))))))

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

(defn success? [{:keys [stack diffs]}]
  (and (empty? stack) (empty? diffs)))

(defn choose-better [defender challenger]
  (cond
    (success? challenger) (reduced challenger)
    (< (search/depth defender)
       (search/depth challenger)) challenger
    :else defender))

(defn choose-best [& problems]
  (if-let [[first-problem & rest-problems] (seq (filter some? problems))]
    (-> (if (success? first-problem)
          first-problem
          (reduce choose-better first-problem rest-problems))
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

(defrecord EqualStarProblem []
  search/Searchable
  (children [problem]
    (loop [{stack :stack :as p} problem]
      (when-let [[k v :as comparison] (peek stack)]
        (condp = k
          ::push (recur (push-path p v))
          ::pop (recur (pop-path p))
          (children comparison (update p :stack pop))))))
  (xform [_]
    (map #(update % :depth (fnil inc 0))))
  search/ExhaustiveSearch
  (stop? [this]
    (success? this))
  search/Combinable
  (combine [self other]
    (update other :best choose-best other self (:best self)))
  search/DepthFirstSearchable
  (depth [this] (- (:depth this 0)
                   (count (:diffs this)))))

(defn equal-star-problem [left right]
  (map->EqualStarProblem {:source     [left right]
                          :stack      (list [left right])
                          :diffs      '()
                          :depth      0
                          :left-path  []
                          :right-path []}))

(defn =* [a b & args]
  (as-> (equal-star-problem a b) $
        (apply search/parallel-depth-first-search $ (or (seq args) [10 4]))
        (choose-best $ (:best $))
        (diff/diff (:diffs $) (:source $))))