(ns advent-of-code-2019.day2
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [advent-of-code-2019.files :as files]
            [advent-of-code-2019.computer :as computer]))

(defn to-operations-vec [code]
  (let [operations-strings (str/split code #",")
        operations-ints (map #(Integer. %) operations-strings)]
    (vec operations-ints)))

(defn operations-vec-to-string [operations]
  (let [operations-strings (map str operations)]
    (str/join "," operations-strings)))

(defn interpret-with-noun-and-verb [code noun verb]
  (let [operations (to-operations-vec code)
        with-noun-changed (assoc operations 1 noun)
        with-noun-and-verb-changed (assoc with-noun-changed 2 verb)
        changed-code (operations-vec-to-string with-noun-and-verb-changed)]
    (computer/interpret-from-string changed-code)))

(defn execute-for-fixed-params [input-file-path]
  (interpret-with-noun-and-verb (files/file-to-string input-file-path) 12 2))

(defn interpret-and-ret-first [code noun verb]
  (get (interpret-with-noun-and-verb code noun verb) 0))

(defn execute-for-changing-params [input-file-path searched-result]
  (let [nouns (range 0 100)
        verbs (range 0 100)
        input-combinations (combo/cartesian-product nouns verbs)
        code (files/file-to-string input-file-path)]
    (first 
     (drop-while 
      (fn [[noun verb]] (not= searched-result (interpret-and-ret-first code noun verb))) input-combinations))))
