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

  "Returns x' constrained to be between `min-value` and `max-value` inclusive.
  
   Boundaries are 0 and 1 if not given."

  ([x]

   (constrain-number 0
                     1
                     x))


  ([min-value max-value x]

   (-> x
       (max min-value)
       (min max-value))))




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




(defn abs

  "Returns the absolute value."

  [x]

  (if (neg? x)
    (- x)
    x))




(defn round

  "Rounds a number."

  [^double x]

  (Math/round x))




(defn random-int

  "Using a given random number generator for numbers between 0 (inclusive) and 1 (exclusive), generates an integer
   between `min-int` (inclusive) and `max-int` (exclusive)."

  ([rng max-int]

   (long (* (rng)
            max-int)))


  ([rng min-int max-int]

   (long (+ (* (rng)
                (- max-int
                   min-int))
             min-int))))




(defn sample-ints

  "Returns a sequence sampling `n` ints from an int array using a random number generator.


   <!> The array will be shuffled and must not be mutated before the sequence is realized."

  ;; TODO. Not needed as reservoir sampling is more efficient ?

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




(defn reservoir-sample-indexes

  "Randomly samples `n-sample` indexes between 0 (inclusive) and `n-total` (exclusive)."

  [rng n-sample n-total]

  (loop [indexes! (transient (vec (range 0
                                         n-sample)))
         i        n-sample]
    (if (>= i
            n-total)
      (persistent! indexes!)
      (let [j (random-int rng
                          0
                          i)]
        (recur (if (< j
                      n-sample)
                 (assoc! indexes!
                         j
                         i)
                 indexes!)
               (inc i))))))
