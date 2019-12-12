(ns advent-of-code-2019.day3
  (:require [clojure.string :as str]
            [clojure.set :as sets]))

(def origin (list 0 0))

(defn make-points 
  ([steps] 
   (make-points steps origin))
  ([steps last-coordinate]
   (if (empty? steps)
     (list last-coordinate)
     (let [step (first steps)
           next-steps (rest steps)
           new-coordinate (step last-coordinate)]
       (lazy-seq (cons last-coordinate (make-points next-steps new-coordinate)))))))

(def operations-codes 
  {\U (fn [[x y]] (list x (inc y)))
   \R (fn [[x y]] (list (inc x) y))
   \D (fn [[x y]] (list x (dec y)))
   \L (fn [[x y]] (list (dec x) y))})

(defn read-code [operations-code]
  (let [operation-code (first operations-code)
        operation (get operations-codes operation-code)
        count (Integer. (apply str (rest operations-code)))]
    (repeat count operation)))

(defn read-steps [steps-string]
  (let [steps-codes (str/split steps-string #",")]
    (apply concat (map read-code steps-codes))))

(def read-route (comp make-points read-steps))

(def read-points (comp set read-route))

(defn get-wires-points [wires-codes-strings]
  (map read-points wires-codes-strings))

(defn get-intersections [wires-codes-strings]
  (let [wires-points (get-wires-points wires-codes-strings)
        intersections (apply sets/intersection wires-points)]
    intersections))

(defn abs [value]
  (if (pos? value) value (- value)))

(defn get-manhatan-dist [point]
  (let [[x y] point
        [orig-x orig-y] origin]
    (+ (abs (- orig-x x)) (abs (- orig-y y)))))

(defn get-manhatan-dists [wires-codes-strings]
  (let [intersections (get-intersections wires-codes-strings)]
    (map get-manhatan-dist intersections)))

(defn get-lowest-manhatan-dist [wires-codes-strings]
  (let [manhatan-dists (get-manhatan-dists wires-codes-strings)]
    (apply min (filter #(not= % 0) manhatan-dists))))

(defn get-lowest-manhatan-dist-from-file [file-path]
  (with-open [reader (clojure.java.io/reader file-path)]
    (get-lowest-manhatan-dist (line-seq reader))))

(comment
  (get-lowest-manhatan-dist-from-file "/home/artur/Pulpit/day3-input"))