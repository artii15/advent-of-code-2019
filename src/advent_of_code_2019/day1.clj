(ns advent-of-code-2019.day1
  (:gen-class))

(def calc-fuel-requirement (comp #(- % 2) int #(Math/floor %) #(/ % 3)))

(defn calc-fuel-requirements [modules-masses] (map calc-fuel-requirement modules-masses))

(defn calc-total-fuel-requirement [modules-masses]
  (reduce + 0 (calc-fuel-requirements modules-masses)))

(defn strings-to-ints [strings] (map #(Integer. %) strings))

(defn calculate-fuel-req-from-file [file-path]
  (with-open [input (clojure.java.io/reader file-path)]
    ((comp calc-total-fuel-requirement strings-to-ints) (line-seq input))))

(defn -main [& args]
  (calculate-fuel-req-from-file (first args)))