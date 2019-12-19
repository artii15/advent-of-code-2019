(ns advent-of-code-2019.day7
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code-2019.computer :as computer]
            [advent-of-code-2019.files :as files]))

(defn run-amplifier [amplifier input]
  (let [amplifier-with-input (computer/add-input amplifier input)]
    (computer/interpret-and-get-state amplifier-with-input)))

(defn run-next-amplifier [ran-amplifiers next-amplifier]
  (let [last-ran-amplifier (peek ran-amplifiers)
        new-last-amplifier-state (computer/discard-output last-ran-amplifier)
        last-output (computer/get-output last-ran-amplifier)
        next-ran-amplifier (run-amplifier next-amplifier last-output)
        last-amplifier-index (- (count ran-amplifiers) 1)
        ran-amplifiers-with-last-updated (assoc ran-amplifiers last-amplifier-index new-last-amplifier-state)]
    (conj ran-amplifiers-with-last-updated next-ran-amplifier)))

(defn run-phase 
  ([amplifiers] (run-phase amplifiers 0))
  ([amplifiers initial-signal]
   (reduce run-next-amplifier 
           [(run-amplifier (first amplifiers) initial-signal)] 
           (rest amplifiers))))

(defn generate-possible-phase-settings [min-setting max-setting]
  (combo/permutations (range min-setting (inc max-setting))))

(defn generate-amplifiers [amplifier-program phase-settings]
  (reduce #(conj %1 (computer/add-input amplifier-program %2)) [] phase-settings))

(defn read-run-result [amplifiers]
  (let [last-amplifier (peek amplifiers)]
    (computer/get-output last-amplifier)))

(defn run-loop
  ([amplifiers] (run-loop amplifiers 0))
  ([amplifiers signal]
   (if (= (get (get (peek amplifiers) :memory) (get (peek amplifiers) :position)) 99) 
     signal
     (let [amplifiers-after-phase-run (run-phase amplifiers signal)
           updated-last-amplifier (computer/discard-output (peek amplifiers-after-phase-run))
           last-amplifier-index (- (count amplifiers) 1)
           updated-amplifiers (assoc amplifiers-after-phase-run last-amplifier-index updated-last-amplifier)
           last-amplifier-output (computer/get-output (peek amplifiers-after-phase-run))] 
       (recur updated-amplifiers last-amplifier-output)))))

(defn part-1 [file-path]
  (let [code (files/file-to-string file-path)
        amplifier-program (computer/initialize-state-from-string code)
        phase-settings-combos (generate-possible-phase-settings 0 4)
        all-amplifiers-seqs-set (map #(generate-amplifiers amplifier-program %) phase-settings-combos)
        all-amplifiers-seqs-after-run (map run-phase all-amplifiers-seqs-set)
        all-runs-results (map read-run-result all-amplifiers-seqs-after-run)]
    (apply max all-runs-results)))

(defn part-2 [file-path]
  (let [code (files/file-to-string file-path)
        amplifier-program (computer/initialize-state-from-string code)
        phase-settings-combos (generate-possible-phase-settings 5 9)
        all-amplifiers-seqs-set (map #(generate-amplifiers amplifier-program %) phase-settings-combos)
        loops-processing-results (map run-loop all-amplifiers-seqs-set)]
    (apply max loops-processing-results)))

(comment
  (part-1 "/home/artur/Pulpit/advent-inputs/day7-input")
  (part-2 "/home/artur/Pulpit/advent-inputs/day7-input"))