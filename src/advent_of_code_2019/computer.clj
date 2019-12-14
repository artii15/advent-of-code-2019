(ns advent-of-code-2019.computer
  (:require [clojure.string :as str]))

(defn read-param-value [memory param mode]
  (case (str mode)
    "0" (get memory param)
    "1" param))

(defn two-param-compute-op [{memory :memory position :position} params-modes op]
  (let [param-1-mode (nth params-modes 0)
        param-2-mode (nth params-modes 1)
        param-1 (get memory (+ position 1))
        param-2 (get memory (+ position 2))
        result-pos (get memory (+ position 3))
        param-1-value (read-param-value memory param-1 param-1-mode)
        param-2-value (read-param-value memory param-2 param-2-mode)]
    {:memory (assoc memory result-pos (op param-1-value param-2-value))
     :position (+ position 4)}))

(defn add [program-state params-modes]
  (two-param-compute-op program-state params-modes +))

(defn multiply [program-state params-modes]
  (two-param-compute-op program-state params-modes *))

(defn write-input [{memory :memory position :position} _]
  (let [result-pos (get memory (+ position 1))]
    {:memory (assoc memory result-pos (Integer. (read-line)))
     :position (+ position 2)}))

(defn print-mem [{memory :memory position :position} params-modes]
  (let [param-mode (first params-modes)
        param (get memory (+ position 1))
        param-value (read-param-value memory param param-mode)]
    (println param-value)
    {:memory memory
     :position (+ position 2)}))

(def instructions 
  {1 add
   2 multiply
   3 write-input
   4 print-mem})

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

(defn interpret-program [{memory :memory position :position :as program-state}]
  (let [{instruction-code :instruction-code 
         params-modes :params-modes} (read-op-code (get memory position))]
    (if (= instruction-code 99) memory
        (let [update-state (get instructions instruction-code)]
          (recur (update-state program-state params-modes))))))

(defn interpret-from-string [program-string]
  (let [string-op-codes (str/split program-string #",")
        int-op-codes (map #(Integer. %) string-op-codes)
        op-codes-indices (range 0 (count int-op-codes))]
    (interpret-program 
     {:memory (zipmap op-codes-indices int-op-codes)
      :position 0})))

(comment 
  (interpret-from-string "1002,4,3,0,99"))