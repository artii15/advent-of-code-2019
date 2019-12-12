(ns advent-of-code-2019.main
  (:require [clojure.string :as str]
            [advent-of-code-2019.day1 :as day1]
            [advent-of-code-2019.day2 :as day2])
  (:gen-class))

(defn make-file-path [dir-path file-name]
  (let [normalized-dir-path (str/replace dir-path #"/$" "")]
    (str/join "/" [normalized-dir-path file-name])))

(defn -main [& args]
  (let [inputs-directory (first args)
        day1-input (make-file-path inputs-directory "day1-input")
        day2-input (make-file-path inputs-directory "day2-input")]
    (println "Day 1:")
    (println
     "Part 1:"
     (day1/calculate-fuel-req-from-file day1-input day1/calc-fuel-requirement))
    (println
     "Part 2:"
     (day1/calculate-fuel-req-from-file day1-input day1/calc-module-fuel-recursive-sum))
    (println "Day 2:")
    (println
     "Part 1:"
     (day2/execute-for-fixed-params day2-input))
    (println
     "Part 2:"
     (day2/execute-for-changing-params day2-input 19690720))))