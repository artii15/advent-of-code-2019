(ns advent-of-code-2019.day5
  (:require [advent-of-code-2019.computer :as computer]
            [advent-of-code-2019.files :as files]))

(defn part-1 [file-path]
  (computer/interpret-from-string (files/file-to-string file-path)))

(comment
  (part-1 "/home/artur/Pulpit/advent-inputs/day5-input"))