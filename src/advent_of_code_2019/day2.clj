(ns advent-of-code-2019.day1
  (:gen-class))

(require '[clojure.string :as str])

(defn split-code [code-str]
  (str/split code-str #","))

(defn execute-operation [op-codes start-position operation]
  (let [arg-1-idx (op-codes (+ start-position 1))
        arg-2-idx (op-codes (+ start-position 2))
        dest-idx (op-codes (+ start-position 3))
        arg-1 (op-codes arg-1-idx)
        arg-2 (op-codes arg-2-idx)
        dest-value (operation arg-1 arg-2)]
    (assoc op-codes dest-idx dest-value)))

(defn interpret-program 
  ([op-codes] (interpret-program op-codes 0))
  ([op-codes current-position] 
   (let [op-code (op-codes current-position)]
     (case op-code
       99 op-codes
       1 (recur (execute-operation op-codes current-position +) (+ current-position 4))
       2 (recur (execute-operation op-codes current-position *) (+ current-position 4))))))

(defn read-and-interpret [input]
  (let [op-codes (map #(Integer. %) (split-code input))]
    (interpret-program (vec op-codes))))

(defn -main [& args]
  (println (read-and-interpret (slurp (first args)))))