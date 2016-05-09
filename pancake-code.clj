;; This function finds the largest element from the list 
(defn biggest [coll] (apply max coll))
;; This function returns index of largest element from list 
(defn indexofbiggest
[coll] (.indexOf coll (biggest coll)))
;;Pancake sorting that applies sort function to all elements in list
(defn pancakesort
[coll]
(if (> 2 (count coll))
coll
(let [spatula (inc (indexofbiggest
coll))
abovespatula
(take spatula coll)
underspatula
(drop spatula coll)
biggestunder
(reverse (concat (reverse abovespatula)
underspatula))]
(
if ( not ( apply <= coll ) )
(dosync (alter x inc))
)
(conj (vec (pancakesort
(butlast biggestunder)))
(last biggestunder))
)))
;; Counts the number of flips required for sorting to final state
(defn countflips
[coll]
(def x (ref 0))
(pancakesort
coll)@x)
;;Produces all permutations for a given number n
(defn numx [x] ( range 1 (+ x 1)) )
;;Applies counting function to all permutations and times it
(time(apply max (map countflips
(permutations (numx 3)))))
