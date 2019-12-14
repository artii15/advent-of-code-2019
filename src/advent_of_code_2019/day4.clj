(ns advent-of-code-2019.day4)

(defn monothonic? 
  [elements] (apply <= (map int elements)))

(defn get-groups 
  ([elements] (if (empty? elements) [] (get-groups (rest elements) [] [(first elements)])))
  ([elements groups group]
   (if (empty? elements)
     (conj groups group)
     (let [next-element (first elements)
           element-in-group (first group)
           next-elements (rest elements)]
       (if (= next-element element-in-group)
         (recur next-elements groups (conj group next-element))
         (recur next-elements (conj groups group) [next-element]))))))

(defn has-multiples? [elements]
  (not (empty? (filter #(> (count %) 1) (get-groups elements)))))

(defn monothonic-with-multiples? [elements]
  (and (monothonic? elements) (has-multiples? elements)))

(defn part-1 []
  (let [passwords (map str (range 138307 654505))]
    (count 
     (filter monothonic-with-multiples? passwords))))

(defn has-doubles? [elements]
  (not (empty? (filter #(= (count %) 2) (get-groups elements)))))

(defn monothonic-with-doubles? [elements]
  (and (monothonic? elements) (has-doubles? elements)))

(defn part-2 []
  (let [passwords (map str (range 138307 654505))]
    (count
     (filter monothonic-with-doubles? passwords))))