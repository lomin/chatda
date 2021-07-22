(ns me.lomin.chatda.a-star-test
  ^{:doc "This test is based on
  https://algorithms.discrete.ma.tum.de/graph-algorithms/spp-a-star/index_en.html"}
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

(defn node [graph letter-or-name]
  (get graph (get graph letter-or-name)))

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
           (incoming-edges g :London)))

    (is (= [[[:i :a] 464]]
           (outgoing-edges g :Dublin)))))

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
  (is (= 20
         (manhattan-distance [0 0] [10 10])
         (manhattan-distance [10 10] [0 0])))

  (let [g (read-graph "resources/cities.txt")]
    (is (= 202
           (manhattan-distance g :London :Berlin)
           (manhattan-distance g :Berlin :London)))))

(defn children [this]
  (let [g (:graph this)]
    (map (fn [[[_ to] costs]]
           (-> this
               (a-star/inc-costs costs)
               (assoc :current (node g to))
               (update :path conj (node g to))))
         (outgoing-edges g (:current this)))))

(a-star/def-a-star CityTravelProblem [graph current target]
  a-star/AStar
  (forward-costs [_] (manhattan-distance graph current target))
  (a-star-identity [_] current)
  search/Searchable
  (children [this] (a-star/with-children (children this)))
  (stop [this _] (a-star/with-stop (and (= current target) this)))
  (combine [_ other] other)
  search/AsyncSearchable
  (combine-async [this other]
    (a-star/with-combine-async this other)))

(defn city-travel-problem [g from to]
  (a-star/init (map->CityTravelProblem {:graph   g
                                        :current (node g from)
                                        :target  (node g to)
                                        :path    [from]})))

(defn shortest-travel
  ([from to] (shortest-travel from to {}))
  ([from to options]
   (as-> (read-graph "resources/cities.txt") $
         (city-travel-problem $ (node $ from) (node $ to))
         (search/search (merge $ {:timeout 1000} options))
         (:path $))))

(deftest shortest-travel-test
  (let [parallel-options {:timeout     1000
                          :parallelism 4
                          :chan-size   4}
        sequential-options {:timeout     1000
                            :parallelism 1
                            :chan-size   1}]
    (is (= [:a]
           (shortest-travel :London :London parallel-options)
           (shortest-travel :London :London sequential-options)))
    (is (= [:a :i]
           (shortest-travel :London :Dublin parallel-options)
           (shortest-travel :London :Dublin sequential-options)))
    (is (= [:a :f :b]
           (shortest-travel :London :Berlin parallel-options)
           (shortest-travel :London :Berlin sequential-options)))
    (is (= [:i :a :f :b :g :d]
           (shortest-travel :Dublin :Kiev parallel-options)
           (shortest-travel :Dublin :Kiev {:timeout     1000
                                           :parallelism 1
                                           :chan-size   1})))
    (is (= [:j :b :g]
           (shortest-travel :Vienna :Minsk parallel-options)
           (shortest-travel :Vienna :Minsk sequential-options)))))