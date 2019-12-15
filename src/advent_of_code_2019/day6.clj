(ns advent-of-code-2019.day6
  (:require [clojure.string :as str]
            [advent-of-code-2019.files :as files]))

(defn line-to-nodes-pair [line]
  (let [[prev nxt] (str/split line #"\)")]
    {:prev prev :nxt nxt}))

(defn lines-to-nodes-pairs [lines]
  (map line-to-nodes-pair lines))

(defn update-predecessors-list [predecessors-list {prev :prev nxt :nxt}]
  (let [node-predecessors (get predecessors-list nxt [])
        updated-node-predecessors (conj node-predecessors prev)]
    (assoc predecessors-list nxt updated-node-predecessors)))

(defn nodes-pairs-to-predecessors-list [nodes-pairs]
  (reduce update-predecessors-list {} nodes-pairs))

(def lines-to-predecessors-list (comp nodes-pairs-to-predecessors-list lines-to-nodes-pairs))

(defn sum-numbers [numbers]
  (reduce + 0 numbers))
(defn count-all-predecesors-of [predecessors-list node]
  (let [direct-predecessors (get predecessors-list node)
        indirect-predecessors-counts (map #(count-all-predecesors-of predecessors-list %1) direct-predecessors)]
    (+ (count direct-predecessors) (sum-numbers indirect-predecessors-counts))))

(defn total-number-of-orbits [predecessors-list]
  (reduce #(+ %1 (count-all-predecesors-of predecessors-list %2)) 0 (keys predecessors-list)))

(defn part-1 [file-path]
  (files/with-file-lines file-path 
    (fn [lines]
      (let [predecessors-list (lines-to-predecessors-list lines)]
        (total-number-of-orbits predecessors-list)))))

(comment 
  (count-all-predecesors-of {:a [:b] :b [:c :d]} :a)
  (count nil)
  (nodes-pairs-to-predecessors-list 
   [{:prev :a :nxt :b} {:prev :b :nxt :c} {:prev :a :nxt :c}])
  (total-number-of-orbits {:a [:b :c] :b [:d :e]})
  (part-1 "/home/artur/Pulpit/advent-inputs/day6-input"))