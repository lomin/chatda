(ns me.lomin.chatda.a-star
  (:require [me.lomin.chatda.search :as search]))

(defprotocol AStarInternal
  (get-costs [self])
  (get-back+forward-costs [self])
  (seen [self]))

(defprotocol AStar
  (forward-costs [self])
  (a-star-identity [self]))

(defn calculate-back+forward-costs [p]
  (+ (or (get-costs p) 0) (forward-costs p)))

(defn inc-costs [p costs]
  (update p :a-star:costs + costs))

(defn depth [priority]
  (get priority 1))

(defn split-at-searchable-protocol [body]
  (split-with #(not= #'search/Searchable (and (symbol? %) (resolve %))) body))

(defmacro def-a-star [name fields & body]
  (let [costs (symbol :a-star:costs)
        best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)
        seen (symbol :a-star:seen)
        back+forward-costs (symbol :a-star:back+forward-costs)
        [first-body-part [_ & more]] (split-at-searchable-protocol body)
        body-with-priority (concat first-body-part
                                   `(search/Searchable
                                      (priority [_#] ~priority)
                                      ~@more))
        fields* (into fields [costs back+forward-costs best-costs priority seen])]
    `(defrecord ~name ~fields*
       AStarInternal
       (get-costs [_#] ~costs)
       (get-back+forward-costs [_#] ~back+forward-costs)
       (seen [_#] ~seen)
       ~@body-with-priority)))

(defmacro with-children [& body]
  (let [best-costs (symbol :a-star:best-costs)
        back+forward-costs (symbol :a-star:back+forward-costs)]
    `(when (< ~back+forward-costs (deref ~best-costs))
       (lazy-seq (do ~@body)))))

(defmacro with-stop [& body]
  (let [best-costs (symbol :a-star:best-costs)
        back+forward-costs (symbol :a-star:back+forward-costs)]
    `(when-let [result# (do ~@body)]
       (swap! ~best-costs min ~back+forward-costs)
       result#)))

(defn best-cost-xform [best-costs]
  (remove #(<= (deref best-costs) (get-back+forward-costs %))))

(defn fork-seen-xform []
  (map #(assoc % :a-star:seen (volatile! @(seen %)))))

(defn priority-xform []
  (map #(update %
                :a-star:priority
                (fn [priority]
                  [(get-back+forward-costs %) (dec (depth priority))]))))

(defn back+forward-costs-xform []
  (map #(assoc % :a-star:back+forward-costs (calculate-back+forward-costs %))))

(defn filter-new-or-better-problems-xform []
  (filter #(let [back+forward-costs (get-back+forward-costs %)
                 seen (seen %)
                 a-star-identity (a-star-identity %)]
             (when (< back+forward-costs (get @seen a-star-identity Integer/MAX_VALUE))
               (vswap! seen assoc a-star-identity back+forward-costs)))))

(defmacro with-xform [& body]
  (let [best-costs (symbol :a-star:best-costs)
        body* (when (seq body) (list (cons 'do body)))]
    `(comp
       (back+forward-costs-xform)
       (filter-new-or-better-problems-xform)
       ~@body*
       (priority-xform)
       (best-cost-xform ~best-costs))))

(defmacro with-xform-async [& body]
  (let [best-costs (symbol :a-star:best-costs)
        body* (when (seq body) (list (cons 'do body)))]
    `(comp (best-cost-xform ~best-costs)
           (fork-seen-xform)
           ~@body*)))

(defmacro with-combine-async
  "Ensures that:
   1. when only one of `this` and `other` stopped, it will be `other` that stopped
   2. when both `this` and `other` stopped,
      `(< (get-back+forward-costs other) (get-back+forward-costs this))`
   In this way, `other` can be returned as a default value (which it
   is, if there is no body at all)."
  [this other & body]
  (let [body* (if (seq body) (cons 'do body) other)]
    `(if (::sorted (meta ~this))
       ~body*
       (if (and (search/stop ~this (search/children ~this))
                (or (not (search/stop ~other (search/children ~other)))
                    (< (get-back+forward-costs ~this)
                       (get-back+forward-costs ~other))))
         (search/combine-async (vary-meta ~other assoc ::sorted true) ~this)
         ~body*))))

(defn init [p]
  (merge p {:compare                   compare
            :a-star:costs              0
            :a-star:seen               (volatile! {(a-star-identity p)
                                                   (calculate-back+forward-costs p)})
            :a-star:back+forward-costs 0
            :a-star:priority           [0 0]
            :a-star:best-costs         (atom Integer/MAX_VALUE)}))