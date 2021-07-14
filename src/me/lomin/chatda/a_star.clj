(ns me.lomin.chatda.a-star
  (:require [me.lomin.chatda.search :as search]))

(defprotocol CostPredictable
  (back+forward-costs [self]))

(defn costs [priority]
  (get priority 0))

(defn depth [priority]
  (get priority 1))

(defn split-at-searchable-protocol [body]
  (split-with #(not= #'search/Searchable (and (symbol? %) (resolve %))) body))

(defmacro def-a-star [name fields & body]
  (let [best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)
        [first-body-part [_ & more]] (split-at-searchable-protocol body)
        body-with-priority (concat first-body-part
                                   `(search/Searchable
                                      (priority [_#] ~priority)
                                      ~@more))
        fields* (into fields [best-costs priority])]
    `(defrecord ~name ~fields*
       ~@body-with-priority)))

(defmacro with-children [& body]
  (let [best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)]
    `(when (< (costs ~priority) (deref ~best-costs))
       (lazy-seq (do ~@body)))))

(defmacro with-stop [& body]
  (let [best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)]
    `(when-let [result# (do ~@body)]
       (swap! ~best-costs min (costs ~priority))
       result#)))

(defn best-cost-xform [best-costs]
  (remove #(<= (deref best-costs) (costs (:a-star:priority %)))))

(defn priority-x-form []
  (map #(update %
                :a-star:priority
                (fn [priority]
                  [(back+forward-costs %) (dec (depth priority))]))))

(defmacro with-xform [& body]
  (let [best-costs (symbol :a-star:best-costs)
        body* (when (seq body) (list (cons 'do body)))]
    `(comp
       ~@body*
       (priority-x-form)
       (best-cost-xform ~best-costs))))

(defmacro with-xform-async [& body]
  (let [best-costs (symbol :a-star:best-costs)]
    (if (seq body)
      `(comp (best-cost-xform ~best-costs) (do ~@body))
      `(best-cost-xform ~best-costs))))

(defmacro with-combine-async
  "Ensures that:
   1. when only one of `this` and `other` stopped, it will be `other` that stopped
   2. when both `this` and `other` stopped,
      `(< (back+forward-costs other) (back+forward-costs this))`
   In this way, `other` can be returned as a default value (which it
   is, if there is no body at all)."
  [this other & body]
  (let [body* (if (seq body) (cons 'do body) other)]
    `(if (::sorted (meta ~this))
       ~body*
       (if (and (search/stop ~this (search/children ~this))
                (or (not (search/stop ~other (search/children ~other)))
                    (< (back+forward-costs ~this)
                       (back+forward-costs ~other))))
         (search/combine-async (vary-meta ~other assoc ::sorted true) ~this)
         ~body*))))

(defn init [p]
  (merge p {:compare           compare
            :a-star:priority   [0 0]
            :a-star:best-costs (atom Integer/MAX_VALUE)}))