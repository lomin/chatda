(ns me.lomin.chatda.a-star
  (:require [me.lomin.chatda.search :as search]))

(defprotocol ForwardCostPredictable
  (heuristic [self]))

(defmacro def-a-star [name fields & body]
  (let [best-costs (symbol :best-costs)
        costs (symbol :costs)
        complete-costs (symbol :complete-costs)
        depth (symbol :depth)
        priority (symbol :priority)
        fields* (into fields [best-costs costs complete-costs depth priority])]
    `(defrecord ~name ~fields*
       ~@body
       search/Prioritizable
       (priority [_] ~priority))))

(defmacro with-children [& body]
  (let [best-costs (symbol :best-costs)
        complete-costs (symbol :complete-costs)]
    `(when (< ~complete-costs (deref ~best-costs))
       ~@body)))

(defmacro with-stop [& body]
  (let [best-costs (symbol :best-costs)
        costs (symbol :costs)]
    `(when-let [result# (do ~@body)]
       (swap! ~best-costs min ~costs)
       result#)))

(defmacro with-xform [& body]
  (let [best-costs (symbol :best-costs)]
    `(comp
       (do ~@body)
       (map #(let [$complete-costs# (+ (:costs %) (heuristic %))]
               (-> %
                   (assoc :complete-costs $complete-costs#)
                   (assoc :priority [$complete-costs# (:depth %)]))))
       (remove #(<= (deref ~best-costs) (:complete-costs %)))
       (map #(update % :depth (fnil dec 0))))))

(defmacro with-xform-async [& body]
  (let [best-costs (symbol :best-costs)]
    `(comp (remove #(<= (deref ~best-costs) (:complete-costs % 0)))
           (do ~@body))))

(defn init [p]
  (merge p {:depth          0
            :costs          0
            :complete-costs 0
            :priority       0
            :best-costs     (atom Integer/MAX_VALUE)}))