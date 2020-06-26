(ns me.lomin.chatda.diff
  (:require [com.rpl.specter :as s]
            [clojure.walk :as walk]
            [lambdaisland.deep-diff.diff :refer [->Mismatch
                                                 ->Deletion
                                                 ->Insertion]]))

(def selector-mapping {::nil  (constantly s/AFTER-ELEM)
                       :set   #(s/set-elem %)
                       :m-key #(s/map-key %)
                       :m-val #(s/keypath %)
                       :index #(s/nthpath %)})

(defn step->selector [[tag v]]
  ((selector-mapping tag) v))

(defn path->selectors [path]
  (mapv step->selector path))

(def sort-mapping
  (into {}
        (map-indexed (fn [i k] [k i])
                     [nil ::nil :m-key :set :m-val :index])))

(defn compare-tags [l r]
  (- (sort-mapping r) (sort-mapping l)))

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
  (let [[l r] (first-different-tags left-path right-path)]
    (compare-tags l r)))

(defn upsert-selector [path-segment inner-selector]
  [(s/nthpath 2)
   (s/multi-path (s/if-path [s/ALL (s/nthpath 1) (s/pred= path-segment)]
                            [(s/terminal identity)]
                            [(s/terminal #(conj % [::node path-segment []]))])
                 [s/ALL (s/selected? (s/nthpath 1) (s/pred= path-segment)) inner-selector])])

(defn path->selector-tree [tree [left-path right-path]]
  (s/multi-transform (reduce (fn [selector path-segment]
                               (upsert-selector path-segment selector))
                             [(s/nthpath 2)
                              (s/terminal #(conj % [::leaf right-path]))]
                             (reverse left-path))
                     tree))

(defn none? [x] (or (nil? x) (= x :com.rpl.specter.impl/NONE)))

(defn diff-obj [right left]
  (cond
    (none? left) (->Insertion right)
    (none? right) (->Deletion left)
    :else (->Mismatch left right)))

(defn node-of? [t x] (and (seqable? x) (= (first x) t)))

(defn selector-tree->diff-selector [right-source selector-tree]
  (walk/postwalk (fn [x]
                   (cond (node-of? ::node x)
                         (let [[_ path children] x]
                           (if (seq path)
                             [(step->selector path) (apply s/multi-path children)]
                             (apply s/multi-path (s/terminal identity) children)))

                         (node-of? ::leaf x)
                         (s/terminal (partial diff-obj
                                              (s/select-first (path->selectors (second x))
                                                              right-source)))

                         :else x))
                 selector-tree))


(defn diff [{fails :fails [left-source right-source] :source}]
  (as-> (sort compare-paths fails) $
        (reduce path->selector-tree [::node [] []] $)
        (selector-tree->diff-selector right-source $)
        (s/multi-transform $ left-source)))