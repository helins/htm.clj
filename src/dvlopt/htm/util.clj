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
  (/ (reduce (fn numerator-product [numerator x]
               (*' numerator
                   x))
             1
             (range n
                    (- n
                       k)
                    -1))
     (factorial k)))




(defn constrain-number

  "Returns x' constrained to be between `min-value` and `max-value` inclusive."

  [min-value max-value x]

  (-> x
      (max min-value)
      (min max-value)))




(defn normalize

  "Normalizes a numerical value between `min-value` and `max-value`.
  
   Values out of range are treated as extreme values."

  [min-value max-value value]

  (let [value-range (- max-value
                       min-value)]
    (/ (- (constrain-number min-value
                            max-value
                            value)
          min-value)
       value-range)))




(defn round

  "Rounds a number."

  [^double x]

  (Math/round x))




(defn sample-ints

  "Returns a sequence sampling `n` ints from an int array using a random number generator.

   The array must not be mutated before the sequence is realized.

  
   <!> The array will be shuffled."

  [rng n ^ints array-int]

  (let [i-last (dec (count array-int))]
    (dotimes [i-sample n]
      (let [i-bit    (aget array-int
                           i-sample)
            j-sample (+ i-sample
                        (round (* (rng)
                                  (- i-last
                                     i-sample))))
            j-bit    (aget array-int
                           j-sample)]
        (aset-int array-int
                  i-sample
                  j-bit)
        (aset-int array-int
                  j-sample
                  i-bit)))
    (take n
          array-int)))
