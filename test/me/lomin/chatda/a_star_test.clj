(ns me.lomin.chatda.a-star-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.a-star :as a-star]))

(defn make-coordinates [line]
  (let [[_ x y] (string/split line #"\s")]
    [(clojure.edn/read-string x)
     (clojure.edn/read-string y)]))

(defn make-edge [m line]
  (let [[_ from to distance] (string/split line #"\s")]
    [[(get m (clojure.edn/read-string from))
      (get m (clojure.edn/read-string to))]
     (clojure.edn/read-string distance)]))

(defn insert-name [m line]
  (let [[_ index-str letter name] (string/split line #"\s")
        index (clojure.edn/read-string index-str)]
    (-> m
        (assoc (keyword letter) index)
        (assoc (keyword name) index)
        (assoc index (keyword letter)))))

(defn read-graph [path]
  (with-open [rdr (clojure.java.io/reader path)]
    (reduce (fn [m [t :as line]]
              (condp = t
                \% (insert-name m line)
                \n (update m :coordinates conj (make-coordinates line))
                \e (update m :edges conj (make-edge m line))
                m))
            {:coordinates []
             :edges       {}}
            (line-seq rdr))))

(def node nil)
(defmulti node (fn [x & args] (mapv type (cons x args))))
(defmethod node [clojure.lang.IPersistentMap clojure.lang.Keyword] [g letter-or-name]
  (get g (get g letter-or-name)))
(defmethod node [clojure.lang.IPersistentMap java.lang.Long] [g index]
  (get-in g index))

(defn index [g $node]
  (get g (node g $node)))

(defn coordinates [g $node]
  (get-in g [:coordinates (index g $node)]))

(defn edges [g $node f]
  (filter (comp (partial = (node g $node))
                f
                first)
          (:edges g)))

(defn outgoing-edges [g $node]
  (edges g $node first))

(defn incoming-edges [g $node]
  (edges g $node second))

(deftest reads-cities-graph-test
  (let [g (read-graph "resources/cities.txt")]
    (is (= 10 (count (:coordinates g))))
    (is (= 28 (count (:edges g))))
    (is (= [[[:a :h] 1435]
            [[:a :f] 343]
            [[:a :i] 464]]
           (outgoing-edges g :London)))
    (is (= [[[:h :a] 1435]
            [[:f :a] 343]
            [[:i :a] 464]]
           (incoming-edges g :London)))))

(defn abs [number]
  (if (pos? number)
    number
    (* -1 number)))

(defn manhattan-distance
  ([g from to]
   (manhattan-distance (coordinates g from) (coordinates g to)))
  ([[x0 y0] [x1 y1]]
   (+ (abs (- x0 x1))
      (abs (- y0 y1)))))

(deftest manhattan-distance-test
  (is (= 20 (manhattan-distance [0 0] [10 10])))
  (is (= 20 (manhattan-distance [10 10] [0 0])))

  (let [g (read-graph "resources/cities.txt")]
    (is (= 202 (manhattan-distance g :London :Berlin)))
    (is (= 202 (manhattan-distance g :Berlin :London)))))

(defn children [this]
  (let [g (:graph this)]
    (map (fn [[[_ to] costs]]
           (-> this
               (update :costs + costs)
               (assoc :current (node g to))
               (update :path conj (node g to))))
         (outgoing-edges g (:current this)))))

(a-star/def-a-star CityTravelProblem [costs graph current target seen]
  a-star/CostPredictable
  (back+forward-costs [_]
    (+ costs (manhattan-distance graph current target)))
  search/Searchable
  (children [this]
    (when (and (not (search/stop this))
               (< costs (get @seen current Integer/MAX_VALUE)))
      (a-star/with-children
        (vswap! seen assoc current costs)
        (children this))))
  (xform [_]
    (a-star/with-xform))
  search/AsyncSearchable
  (xform-async [_]
    (a-star/with-xform-async
      (map #(assoc % :seen (volatile! @(:seen %))))))
  search/ExhaustiveSearch
  (stop [this]
    (a-star/with-stop
      (and (= current target) this)))
  search/Combinable
  (combine [_ other] other)
  (combine-async [this other]
    (a-star/with-combine-async this other)))

(defn city-travel-problem [g from to]
  (-> (map->CityTravelProblem {:costs   0
                               :graph   g
                               :current (node g from)
                               :target  (node g to)
                               :seen    (volatile! {})
                               :path    [from]})
      (a-star/init)))

(defn shortest-travel
  ([from to] (shortest-travel from to {}))
  ([from to options]
   (as-> (read-graph "resources/cities.txt") $
         (city-travel-problem $ (node $ from) (node $ to))
         (search/parallel-depth-first-search $
                                             (merge {:parallelism 1 :chan-size 1 :timeout 1000}
                                                    options))
         (:path $))))

(deftest shortest-travel-test
  (let [options {:timeout     1000
                 :parallelism 4
                 :chan-size   4}]
    (is (= [:a] (shortest-travel :London :London options)))
    (is (= [:a :i] (shortest-travel :London :Dublin options)))
    (is (= [:a :f :b] (shortest-travel :London :Berlin options)))
    (is (= [:i :a :f :b :g :d] (shortest-travel :Dublin :Kiev options)))
    (is (= [:j :b :g] (shortest-travel :Vienna :Minsk options)))))