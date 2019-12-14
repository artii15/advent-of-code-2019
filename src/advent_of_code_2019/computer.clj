(ns advent-of-code-2019.computer)

(def instruction-length 2)

(def read-instruction-code
  (comp #(Integer. %) #(apply str %) reverse #(take instruction-length %) reverse))

(def read-params-modes
  (comp #(concat % (repeat \0)) #(drop instruction-length %) reverse))

(comment
  (read-instruction-code "2")
  (take 10 (read-params-modes "10202")))

(defn read-op-code [op-code]
  (let [instruction-code (read-instruction-code op-code)
        ]))