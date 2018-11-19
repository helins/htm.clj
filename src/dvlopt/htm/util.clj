(ns dvlopt.htm.util

  ""

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
