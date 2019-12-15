(ns advent-of-code-2019.files
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def file-to-string (comp str/trim-newline slurp)) 

(defn with-file-lines [file-path func]
  (with-open [reader (io/reader file-path)]
    (func (line-seq reader))))