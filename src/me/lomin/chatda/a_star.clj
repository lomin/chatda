(ns me.lomin.chatda.a-star
  (:require [me.lomin.chatda.search :as search]))

(defprotocol AStarInternal
  (get-costs [self])
  (get-best-costs [self])
  (get-back+forward-costs [self])
  (seen [self]))

(defprotocol AStar
  (forward-costs [self])
  (a-star-identity [self])
  (goal? [this]))

(defn calculate-back+forward-costs [p]
  (+ (or (get-costs p) 0) (forward-costs p)))

(defn inc-costs [p costs]
  (update p :a-star:costs + costs))

(defn depth [priority]
  (get priority 1))

(defn split-at-searchable-protocol [body]
  (split-with #(not= #'search/SearchableNode (and (symbol? %) (resolve %))) body))

(defmacro def-a-star [name fields & body]
  (let [costs (symbol :a-star:costs)
        best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)
        seen (symbol :a-star:seen)
        back+forward-costs (symbol :a-star:back+forward-costs)
        [first-body-part [_ & more]] (split-at-searchable-protocol body)
        body-with-priority (concat first-body-part
                                   `(search/SearchableNode
                                      (priority [_#] ~priority)
                                      ~@more))
        fields* (into fields [costs back+forward-costs best-costs priority seen])]
    `(defrecord ~name ~fields*
       AStarInternal
       (get-costs [_#] ~costs)
       (get-best-costs [_#] ~best-costs)
       (get-back+forward-costs [_#] ~back+forward-costs)
       (seen [_#] ~seen)
       ~@body-with-priority)))

(defmacro with-stop [this & body]
  (let [best-costs (symbol :a-star:best-costs)
        back+forward-costs (symbol :a-star:back+forward-costs)]
    `(if (or (<= (deref ~best-costs) ~back+forward-costs)
             (goal? ~this))
       ~this
       (when-let [result# (do ~@body)]
         (swap! ~best-costs min ~back+forward-costs)
         result#))))

(defn best-cost-xform []
  (remove #(<= (deref (get-best-costs %)) (get-back+forward-costs %))))

(defn fork-seen-xform []
  (map #(assoc % :a-star:seen (volatile! @(seen %)))))

(defn priority-xform []
  (map #(update %
                :a-star:priority
                (fn [priority]
                  [(get-back+forward-costs %) (dec (depth priority))]))))

(defn back+forward-costs-xform []
  (map #(assoc % :a-star:back+forward-costs (calculate-back+forward-costs %))))

(defn filter-new-or-better-nodes-xform []
  (filter #(let [back+forward-costs (get-back+forward-costs %)
                 seen (seen %)
                 a-star-identity (a-star-identity %)]
             (when (< back+forward-costs (get @seen a-star-identity Integer/MAX_VALUE))
               (vswap! seen assoc a-star-identity back+forward-costs)))))

(defmacro with-xform [& body]
  (let [body* (when (seq body) (list (cons 'do body)))]
    `(comp
       (back+forward-costs-xform)
       (filter-new-or-better-nodes-xform)
       ~@body*
       (priority-xform)
       (best-cost-xform))))

(defmacro with-xform-async [& body]
  (let [body* (when (seq body) (list (cons 'do body)))]
    `(comp (best-cost-xform)
           (fork-seen-xform)
           ~@body*)))

(defn compare-features [feature+compare-xs a b]
  (reduce (fn [[a b :as comparison] [feature compare*]]
            (let [result ((or compare* search/smaller-is-better)
                          (feature a) (feature b))]
              (cond (= 0 result) comparison
                    (neg? result) (reduced [a b])
                    :else (reduced [b a]))))
          [a b]
          feature+compare-xs))

(def a-star-better-features
  [[(comp boolean goal?) search/larger-is-better]
   [get-back+forward-costs search/smaller-is-better]
   [(comp depth search/priority) search/larger-is-better]])

(defn choose-better [a b]
  (first (compare-features a-star-better-features a b)))

(defn init
  ([root-node] (init root-node nil))
  ([root-node custom-config]
   (merge {:root-node        (merge root-node
                                    {:a-star:costs              0
                                     :a-star:seen               (volatile!
                                                                  {(a-star-identity root-node)
                                                                   (calculate-back+forward-costs root-node)})
                                     :a-star:back+forward-costs 0
                                     :a-star:priority           [0 0]
                                     :a-star:best-costs         (atom Integer/MAX_VALUE)})
           :compare-priority search/smaller-is-better
           :search-xf        (with-xform)
           :search-xf-async  (with-xform-async)}
          custom-config)))