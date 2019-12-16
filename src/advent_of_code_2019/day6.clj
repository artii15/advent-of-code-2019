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

(comment 
  (part-1 "/home/artur/Pulpit/advent-inputs/day6-input"))