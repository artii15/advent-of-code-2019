(ns advent-of-code-2019.day1
  (:gen-class))

(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])

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

(defn file-to-string [file-location] 
  ((comp str/trim-newline slurp) file-location))

(defn interpret-with-noun-and-verb [codes noun verb]
  (let [with-noun-changed (assoc codes 1 noun)
        with-noun-and-verb-changed (assoc with-noun-changed 2 verb)]
    (interpret-program with-noun-and-verb-changed)))

(defn read-codes-from-file [input-file-path]
  (let [file-content ((comp str/trim-newline slurp) input-file-path)
        string-codes (split-code file-content)]
    (vec (map #(Integer. %) string-codes))))

(defn execute-for-fixed-params [input-file-path]
  (interpret-with-noun-and-verb (read-codes-from-file input-file-path) 12 2))

(defn interpret-and-ret-first [codes noun verb]
  (first (interpret-with-noun-and-verb codes noun verb)))

(defn execute-for-changing-params [input-file-path searched-result]
  (let [nouns (range 0 100)
        verbs (range 0 100)
        input-combinations (combo/cartesian-product nouns verbs)
        codes (read-codes-from-file input-file-path)]
    (first 
     (drop-while 
      (fn [[noun verb]] (not= searched-result (interpret-and-ret-first codes noun verb))) input-combinations))))

(defn -main [& args]
  (println (execute-for-fixed-params (first args))))