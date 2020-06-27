(ns me.lomin.chatda.diff
  (:require [com.rpl.specter :as s]
            [clojure.walk :as walk]
            [lambdaisland.deep-diff.diff :refer [->Mismatch
                                                 ->Deletion
                                                 ->Insertion]]))

(def root-node [::node #_node|leaf
                [#_[tag v]] #_path-segment
                [] #_children-nodes])
(def tag-nav (s/nthpath 1))
(def noop-terminal-nav (s/terminal identity))
(def children-nav (s/nthpath 2))
(defn leaf-terminal-nav [right-path]
  (s/terminal #(conj % [::leaf right-path])))
(defn node-terminal-nav [path-segment]
  (s/terminal #(conj % [::node path-segment []])))

(def tag-ranking
  (into {}
        (map-indexed (fn [i k] [k i])
                     [nil ::nil :m-key :set :m-val :index])))

(defn compare-tags [l r]
  (- (tag-ranking r) (tag-ranking l)))

(defn extract-tags [path]
  (s/select [s/FIRST s/ALL s/FIRST] path))

(defn tag-seq [path]
  (concat (extract-tags path) (repeat nil)))

(defn first-different-tags [left-path right-path]
  (first (sequence (comp (take-while (fn [[l r]] (not= nil l r)))
                         (remove (fn [[l r]] (= l r))))
                   (map vector
                        (tag-seq left-path)
                        (tag-seq right-path)))))

(defn compare-paths [left-path right-path]
  (let [[left-tag right-tag] (first-different-tags left-path right-path)]
    (compare-tags left-tag right-tag)))

(defn upsert-navigator [inner-navigator path-segment]
  [children-nav
   (s/multi-path (s/if-path [s/ALL tag-nav (s/pred= path-segment)]
                            noop-terminal-nav
                            (node-terminal-nav path-segment))
                 [s/ALL
                  (s/selected? tag-nav (s/pred= path-segment))
                  inner-navigator])])

(defn grow-path-tree [tree [left-path right-path]]
  (s/multi-transform (reduce upsert-navigator
                             [children-nav (leaf-terminal-nav right-path)]
                             (reverse left-path))
                     tree))

(defn none? [x] (or (nil? x) (= x :com.rpl.specter.impl/NONE)))

(defn diff-obj [right left]
  (cond
    (none? left) (->Insertion right)
    (none? right) (->Deletion left)
    :else (->Mismatch left right)))

(def navigator-mapping {::nil  (constantly s/AFTER-ELEM)
                        :set   #(s/set-elem %)
                        :m-key #(s/map-key %)
                        :m-val #(s/keypath %)
                        :index #(s/nthpath %)})

(defn path-segment->navigator [[tag v]]
  ((navigator-mapping tag) v))

(defn node-diff-transformer [[_ path-segment children]]
  (if (seq path-segment)
    [(path-segment->navigator path-segment) (apply s/multi-path children)]
    (apply s/multi-path noop-terminal-nav children)))

(defn path->navigators [path]
  (mapv path-segment->navigator path))

(defn leaf-diff-transformer [[_ right-path] right-source]
  (s/terminal (partial diff-obj
                       (s/select-first (path->navigators right-path)
                                       right-source))))

(defn node-of? [t x] (and (seqable? x) (= (first x) t)))

(defn path-tree->diff-transformer [right-source selector-tree]
  (walk/postwalk #(cond (node-of? ::node %) (node-diff-transformer %)
                        (node-of? ::leaf %) (leaf-diff-transformer % right-source)
                        :else %)
                 selector-tree))

(defn diff [{paths :fails [left-source right-source] :source}]
  (as-> paths $
        (sort compare-paths $)
        (reduce grow-path-tree root-node $)
        (path-tree->diff-transformer right-source $)
        (s/multi-transform $ left-source)))