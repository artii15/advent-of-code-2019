(ns advent-of-code-2019.main
  (:require [clojure.string :as str]
            [advent-of-code-2019.day5 :as day5])
  (:gen-class))

(defn make-file-path [dir-path file-name]
  (let [normalized-dir-path (str/replace dir-path #"/$" "")]
    (str/join "/" [normalized-dir-path file-name])))

(defn -main [& args]
  (let [inputs-directory (first args)
        day5-input (make-file-path inputs-directory "day5-input")]
    (day5/execute day5-input)))