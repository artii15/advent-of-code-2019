(ns advent-of-code-2019.computer
  (:require [clojure.string :as str]))

(defn read-param-value [memory param mode]
  (case (str mode)
    "0" (get memory param)
    "1" param))

(defn two-param-compute-op [{memory :memory position :position stdout :stdout} params-modes op]
  (let [param-1-mode (nth params-modes 0)
        param-2-mode (nth params-modes 1)
        param-1 (get memory (+ position 1))
        param-2 (get memory (+ position 2))
        result-pos (get memory (+ position 3))
        param-1-value (read-param-value memory param-1 param-1-mode)
        param-2-value (read-param-value memory param-2 param-2-mode)]
    {:memory (assoc memory result-pos (op param-1-value param-2-value))
     :position (+ position 4)
     :stdout stdout}))

(defn add [program-state params-modes]
  (two-param-compute-op program-state params-modes +))

(defn multiply [program-state params-modes]
  (two-param-compute-op program-state params-modes *))

(defn write-input [{memory :memory position :position stdout :stdout} _]
  (let [result-pos (get memory (+ position 1))]
    {:memory (assoc memory result-pos (Integer. (read-line)))
     :position (+ position 2)
     :stdout stdout}))

(defn print-mem [{memory :memory position :position stdout :stdout} params-modes]
  (let [param-mode (first params-modes)
        param (get memory (+ position 1))
        param-value (read-param-value memory param param-mode)]
    (println param-value)
    {:memory memory
     :position (+ position 2)
     :stdout (conj stdout param-value)}))

(defn jump-if [{memory :memory position :position stdout :stdout} params-modes with-zero-comparator]
  (let [param-1-mode (nth params-modes 0)
        param-2-mode (nth params-modes 1)
        param-1 (get memory (+ position 1))
        param-2 (get memory (+ position 2))
        param-1-value (read-param-value memory param-1 param-1-mode)
        param-2-value (read-param-value memory param-2 param-2-mode)]
    {:memory memory
     :position (if (with-zero-comparator param-1-value 0) param-2-value (+ position 3))
     :stdout stdout}))

(defn jump-if-true [program-state params-modes]
  (jump-if program-state params-modes not=))

(defn jump-if-false [program-state params-modes] 
  (jump-if program-state params-modes =))

(defn bool-to-int [bool-val]
  (if bool-val 1 0))
(defn compare-and-store [{memory :memory position :position stdout :stdout} params-modes comparator]
  (let [param-1-mode (nth params-modes 0)
        param-2-mode (nth params-modes 1)
        param-1 (get memory (+ position 1))
        param-2 (get memory (+ position 2))
        param-1-value (read-param-value memory param-1 param-1-mode)
        param-2-value (read-param-value memory param-2 param-2-mode)
        result-pos (get memory (+ position 3))]
    {:memory (assoc memory result-pos (bool-to-int (comparator param-1-value param-2-value)))
     :position (+ position 4)
     :stdout stdout}))

(defn less-than [program-state params-modes]
  (compare-and-store program-state params-modes <))

(defn equals [program-state params-modes]
  (compare-and-store program-state params-modes =))

(def instructions 
  {1 add
   2 multiply
   3 write-input
   4 print-mem
   5 jump-if-true
   6 jump-if-false
   7 less-than
   8 equals})

(def instruction-length 2)

(def to-int #(Integer. %))
(def read-instruction-code
  (comp to-int #(apply str %) reverse #(take instruction-length %) reverse str))

(def read-params-modes
  (comp #(map to-int %) #(map str %) #(drop instruction-length %) reverse str))

(defn read-op-code [op-code]
  (let [instruction-code (read-instruction-code op-code)
        params-modes (concat (read-params-modes op-code) (repeat \0))]
    {:instruction-code instruction-code :params-modes params-modes}))

(defn interpret-and-get-state [{memory :memory position :position :as program-state}]
  (let [{instruction-code :instruction-code
         params-modes :params-modes} (read-op-code (get memory position))]
    (if (= instruction-code 99) program-state
        (let [update-state (get instructions instruction-code)]
          (recur (update-state program-state params-modes))))))

(defn interpret-program [program-state]
  (let [final-state (interpret-and-get-state program-state)]
    (get final-state :memory)))

(defn interpret-from-string-and-get-state [program-string]
  (let [string-op-codes (str/split program-string #",")
        int-op-codes (map #(Integer. %) string-op-codes)
        op-codes-indices (range 0 (count int-op-codes))]
    (interpret-and-get-state
     {:memory (zipmap op-codes-indices int-op-codes)
      :position 0
      :stdout []})))

(defn interpret-from-string [program-string]
  (let [final-state (interpret-from-string-and-get-state program-string)]
    (get final-state :memory)))