(ns me.lomin.chatda.a-star
  (:require [me.lomin.chatda.search :as search]))

(defprotocol CostPredictable
  (back+forward-costs [self]))

(defn costs [priority]
  (get priority 0))

(defn depth [priority]
  (get priority 1))

(defmacro def-a-star [name fields & body]
  (let [best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)
        fields* (into fields [best-costs priority])]
    `(defrecord ~name ~fields*
       ~@body
       search/Prioritizable
       (priority [_] ~priority))))

(defmacro with-children [& body]
  (let [best-costs (symbol :a-star:best-costs)
        priority (symbol :a-star:priority)]
    `(when (< (costs ~priority) (deref ~best-costs))
       ~@body)))

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
  [this other & body]
  (let [body* (if (seq body) (list (cons 'do body)) (list other))]
    `(if (::sorted (meta ~this))
       ~@body*
       (if (and (search/stop ~this)
                (or (not (search/stop ~other))
                    (< (back+forward-costs ~this)
                       (back+forward-costs ~other))))
         (search/combine-async (vary-meta ~other assoc ::sorted true) ~this)
         (search/combine-async (vary-meta ~this assoc ::sorted true) ~other)))))

(defn init [p]
  (merge p {:compare compare
            :a-star:priority   [0 0]
            :a-star:best-costs (atom Integer/MAX_VALUE)}))