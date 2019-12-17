(ns advent-of-code-2019.day7
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent-of-code-2019.computer :as computer]
            [advent-of-code-2019.files :as files]))

(defn run-amplifier [code phase-setting input-signal]
  (let [stdin (str phase-setting "\n" input-signal)]
    ((comp #(Integer. %) str/trim) 
     (with-out-str
       (with-in-str stdin (computer/interpret-from-string code))))))

(defn run-phase [code phase-settings]
  (reduce #(run-amplifier code %2 %1) 0 phase-settings))

(defn generate-possible-phase-settings [number-of-amplifiers]
  (combo/permutations (range 0 number-of-amplifiers)))

(defn part-1 [file-path]
  (let [code (files/file-to-string file-path)
        phases-results (map #(run-phase code %) (generate-possible-phase-settings 5))]
    (apply max phases-results)))

(comment
  (part-1 "/home/artur/Pulpit/advent-inputs/day7-input"))