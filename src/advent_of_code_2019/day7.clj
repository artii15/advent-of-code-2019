(ns advent-of-code-2019.day7
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent-of-code-2019.computer :as computer]
            [advent-of-code-2019.files :as files]))

(defn run-amplifier [amplifier-state phase-setting input-signal]
  (let [stdin (str phase-setting "\n" input-signal)]
    (with-in-str stdin (computer/interpret-and-get-state amplifier-state))))

(defn update-amplifiers-states [[signal states] [amplifier-state phase-setting]]
  (let [new-amplifier-state (run-amplifier amplifier-state phase-setting signal)]
    [(peek (get new-amplifier-state :stdout)) 
     (conj states (dissoc new-amplifier-state :stdout))]))

(defn run-phase [amplifiers-states phase-settings initial-signal]
  (reduce update-amplifiers-states 
          [initial-signal []] 
          (map vector amplifiers-states phase-settings)))

(defn generate-possible-phase-settings [number-of-amplifiers]
  (combo/permutations (range 0 number-of-amplifiers)))

(defn part-1 [file-path]
  (let [code (files/file-to-string file-path)
        initial-amplifiers-states (repeat 5 (computer/initialize-state-from-string code))]
    (run-phase initial-amplifiers-states [0 1 2 3 4] 0)))

(comment
  (part-1 "/home/artur/Pulpit/advent-inputs/day7-input"))