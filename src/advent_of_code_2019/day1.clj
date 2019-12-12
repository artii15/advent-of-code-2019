(ns advent-of-code-2019.day1)

(def calc-fuel-requirement (comp #(- % 2) int #(Math/floor %) #(/ % 3)))

(defn calc-module-fuel-requirement-seq [mass]
  (let [required-fuel (calc-fuel-requirement mass)]
    (if (pos? required-fuel)
      (lazy-seq (cons required-fuel (calc-module-fuel-requirement-seq required-fuel)))
      [])))

(defn calc-module-fuel-recursive-sum [module-mass]
  (reduce + 0 (calc-module-fuel-requirement-seq module-mass)))

(defn calc-total-fuel-requirement [modules-masses module-fuel-calculator]
  (reduce + 0 (map module-fuel-calculator modules-masses)))

(defn strings-to-ints [strings] (map #(Integer. %) strings))

(defn calculate-fuel-req-from-file [file-path module-fuel-calculator]
  (with-open [input (clojure.java.io/reader file-path)]
    ((comp #(calc-total-fuel-requirement % module-fuel-calculator) strings-to-ints) (line-seq input))))