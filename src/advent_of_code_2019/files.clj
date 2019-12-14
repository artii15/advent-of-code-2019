(ns advent-of-code-2019.files
  (:require [clojure.string :as str]))

(def file-to-string (comp str/trim-newline slurp)) 