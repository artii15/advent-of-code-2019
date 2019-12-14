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

(defn has-appropriate-groups? [elements is-group-appropriate?]
  ((comp not empty?) (filter is-group-appropriate? (get-groups elements))))

(defn monothonic-with-multiples? [elements]
  (and (monothonic? elements) (has-appropriate-groups? elements #(> (count %) 1))))

(defn part-1 []
  (let [passwords (map str (range 138307 654505))]
    (count 
     (filter monothonic-with-multiples? passwords))))

(defn monothonic-with-doubles? [elements]
  (and (monothonic? elements) (has-appropriate-groups? elements #(= (count %) 2))))

(defn part-2 []
  (let [passwords (map str (range 138307 654505))]
    (count
     (filter monothonic-with-doubles? passwords))))