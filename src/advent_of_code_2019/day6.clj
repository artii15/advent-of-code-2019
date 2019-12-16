(ns advent-of-code-2019.day6
  (:require [clojure.string :as str]
            [advent-of-code-2019.files :as files]))

(defn lines-to-predecessors-map [lines]
  (reduce 
   (fn [predecessors-map line]
     (let [[prev nxt] (str/split line #"\)")]
       (assoc predecessors-map nxt prev))) 
   {} lines))

(defn node-predecessors 
  ([predecessors-map node] 
   (node-predecessors predecessors-map node []))
  ([predecessors-map node predecessors]
   (let [predecessor (get predecessors-map node)]
     (if (nil? predecessor) 
       predecessors 
       (recur predecessors-map predecessor (conj predecessors predecessor))))))

(defn total-number-of-orbits [predecessors-map]
  (let [nodes-predecessors (map #(node-predecessors predecessors-map %) (keys predecessors-map))
        nodes-predecessors-counts (map count nodes-predecessors)]
    (reduce + nodes-predecessors-counts)))

(defn part-1 [file-path]
  (files/with-file-lines file-path 
    (fn [lines]
      (let [predecessors-map (lines-to-predecessors-map lines)]
        (total-number-of-orbits predecessors-map)))))

(defn common-predecessors [predecessors-map node-1 node-2]
  (let [node-1-predecessors (node-predecessors predecessors-map node-1)
        node-2-predecessors (node-predecessors predecessors-map node-2)
        node-2-predecessors-set (set node-2-predecessors)]
    (drop-while #((comp not contains?) node-2-predecessors-set %) node-1-predecessors)))

(defn common-root [predecessors-map node-1 node-2]
  (first (common-predecessors predecessors-map node-1 node-2)))

(defn path-to-predecessor [predecessors searched-predecessor]
  (take-while #(not= searched-predecessor %) predecessors))

(defn orbital-transfers [predecessors-map node-1 node-2]
  (let [node-1-predecessors (node-predecessors predecessors-map node-1)
        node-2-predecessors (node-predecessors predecessors-map node-2)
        root (common-root predecessors-map node-1 node-2)
        from-node-1-to-root (path-to-predecessor node-1-predecessors root)
        from-node-2-to-root (path-to-predecessor node-2-predecessors root)]
    (concat from-node-1-to-root from-node-2-to-root)))

(defn part-2 [file-path]
  (files/with-file-lines file-path
    (fn [lines]
      (let [predecessors-map (lines-to-predecessors-map lines)]
        (count (orbital-transfers predecessors-map "SAN" "YOU"))))))