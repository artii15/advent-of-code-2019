(ns advent-of-code-2019.day5
  (:require [advent-of-code-2019.computer :as computer]
            [advent-of-code-2019.files :as files]))

(defn execute [file-path]
  (computer/interpret-from-string (files/file-to-string file-path)))

(comment
  (execute "/home/artur/Pulpit/advent-inputs/day5-input"))
