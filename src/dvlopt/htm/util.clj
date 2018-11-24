(ns dvlopt.htm.util

  "Utility functions."

  {:author "Adam Helinski"})




;;;;;;;;;;


(defn factorial

  "Computes a factorial."

  [n]

  (when-not (nat-int? n)
    (throw (IllegalArgumentException. (format "Cannot compute factorial of '%s'"
                                              n))))
  (reduce (fn [factorial' x]
            (*' factorial'
                x))
          1
          (range 1
                 (inc n))))




(defn count-combinations

  "Counts the number of combinations of n choose k."

  [n k]

  (when (> k
           n)
    (throw (IllegalArgumentException. "Computing combinations, k cannot be > n")))
  (/ (reduce (fn ??? [numerator x]
               (*' numerator
                   x))
             1
             (range n
                    (- n
                       k)
                    -1))
     (factorial k)))




(defn normalize

  "Normalizes a numerical value between `min-value` and `max-value`.
  
   Values out of range are treated as extreme values."

  [min-value max-value value]

  (let [constrained-value (-> value
                              (max min-value)
                              (min max-value))
        value-range       (- max-value
                             min-value)]
    (/ (- constrained-value
          min-value)
       value-range)))
