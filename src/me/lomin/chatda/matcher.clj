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

(defprotocol Path
  (path [self [left right]]))

(defn equality-partition-set [[a b] & _]
  (hash-set (data/equality-partition a)
            (data/equality-partition b)))

(defmulti children equality-partition-set)

(defn path-tag
  ([tag elem]
   (path-tag tag elem elem))
  ([tag elem val]
   (if (= elem ::diff/nil)
     [::diff/nil]
     [tag val])))

(defmacro with-tags
  ([ta va _ pred tb vb]
   `(if ~pred [[::pop] ~vb [::push ~tb]
               [::pop] ~va [::push ~ta]]
              [[::pop] ~va [::push ~ta]]))
  ([tags values]
   [[::pop] values [::push tags]]))

(extend-type java.util.Set Path
  (path [_ [left right]]
    (with-tags [[:set left] [:set right]]
               [left right])))

(extend-type java.util.Map Path
  (path [_ [[left-key left-value] [right-key right-value]]]
    (with-tags [(path-tag :m-key left-key)
                (path-tag :m-key right-key)]
               [left-key right-key]
               :when (and (not= ::diff/nil left-key)
                          (not= ::diff/nil right-key))
               [(path-tag :m-val left-key)
                (path-tag :m-val right-key)]
               [left-value right-value])))

(extend-type java.util.List Path
  (path [_ [left right index]]
    (with-tags [(path-tag :index left index)
                (path-tag :index right index)]
               [left right])))

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
  (mapcat (partial path col)))

(defn subset-col-children [dis
                           nil-value
                           [left right :as comparison] problem]
  (if (empty? left)
    (list problem)
    (for [l left
          r (if (<= (count left) (count right))
              right
              (conj right nil-value))]
      (let [left* (dis left l)]
        (as-> problem $
              (if (seq left*)
                (update $ :stack conj [left*
                                       (dis right r)])
                $)
              (update $ :stack into (search/transduce-1 (col->path-xf comparison)
                                                        [l r])))))))

(let [set-dis #(disj %1 %2)
      nil-value ::diff/nil]
  (defmethod children #{:set}
    [comparison problem]
    (subset-col-children set-dis nil-value comparison problem)))

(let [map-dis #(dissoc %1 (first %2))
      nil-value [::diff/nil ::diff/nil]]
  (defmethod children #{:map}
    [comparison problem]
    (subset-col-children map-dis nil-value comparison problem)))

(def take-until-both-empty-xf
  (take-while (comp (partial not= [::diff/nil ::diff/nil])
                    (partial take 2))))

(defmethod children #{:sequential}
  [[left-xs right-xs :as comparison] problem]
  (list (update problem
                :stack
                into
                (comp take-until-both-empty-xf
                      (col->path-xf comparison))
                (map vector
                     (concat left-xs (repeat ::diff/nil))
                     (concat right-xs (repeat ::diff/nil))
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

(defn =* [a b & args]
  (as-> (subset-problem a b) $
        (apply search/parallel-depth-first-search $ (or (seq args) [10 4]))
        (find-best $ (:best $))
        (diff/diff $)))