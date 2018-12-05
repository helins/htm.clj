(ns dvlopt.htm.math
  
  "Miscellaneous math functions."

  {:author "Adam Helinski"})




;;;;;;;;;;


(defn abs

  "Returns the absolute value."

  [x]

  (if (neg? x)
    (- x)
    x))




(defn factorial

  "Computes a factorial."

  [^long n]

  (when-not (nat-int? n)
    (throw (IllegalArgumentException. (format "Cannot compute factorial of '%s', must be <= 0."
                                              n))))
  (reduce (fn next-term [factorial' x]
            (*' factorial'
                x))
          1
          (range 1
                 (inc n))))




(defn fit-to-range

  "Returns value' constrained to be between `min-value` and `max-value` inclusive.
  
   Boundaries are 0 and 1 if not given."

  ([value]

   (fit-to-range 0
                 1
                 value))


  ([max-value value]

   (fit-to-range 0
                 max-value
                 value))


  ([min-value max-value value]

   (-> value
       (max min-value)
       (min max-value))))




(defn mean

  "Computes the mean of a sequence."

  [sq]

  (loop [i   0
         sum 0
         sq' sq]
    (if-let [sq'2 (seq sq')]
      (recur (inc i)
             (+ sum
                (first sq'2))
             (rest sq'2))
      (if (zero? i)
        0
        (/ sum
           i)))))





(defn min-max-normalization

  "Normalizes a numerical value between `min-value` and `max-value`.
  
   Values out of range are treated as extreme values."

  [min-value max-value value]

  (let [value-range (- max-value
                       min-value)]
    (/ (- (fit-to-range min-value
                        max-value
                        value)
          min-value)
       value-range)))




(defn n-combinations

  "Counts the number of combinations of n choose k."

  [^long n ^long k]

  (when (> k
           n)
    (throw (IllegalArgumentException. (format "Cannot compute n choose k because k (%d) > n (%d)"
                                              k
                                              n))))
  (/ (reduce (fn numerator-product [numerator x]
               (*' numerator
                   x))
             1
             (range n
                    (- n
                       k)
                    -1))
     (factorial k)))




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




(defn durstenfeld-shuffle

  "Durstenfeld version of the Fisher-Yates shuffle."

  [rng collection]

  (let [collection! (transient (vec collection))
        size        (count collection!)
        limit       (- size
                       2)]
    (loop [i 0]
      (if (<= i
              limit)
        (let [j (random-int rng
                            i
                            size)
              i-val (get collection!
                         i)]
          (assoc! collection!
                  i
                  (get collection!
                       j))
          (assoc! collection!
                  j
                  i-val)
          (recur (inc i)))
        (persistent! collection!)))))




(defn reservoir-sample-ints

  "Randomly samples `n-sample` integers between `min-int` (inclusive, defaults to 0) and `max-int` (exclusive)."

  ([rng max-int n-sample]

   (reservoir-sample-ints rng
                          0
                          max-int
                          n-sample))


  ([rng ^long min-int ^long max-int n-sample]

   (let [ints! (transient (vec (range min-int
                                      n-sample)))]
     (loop [i n-sample]
       (if (< i
              max-int)
         (let [j (random-int rng
                             0
                             i)]
           (when (< j
                    n-sample)
             (assoc! ints!
                     j
                     i))
           (recur (inc i)))
         (persistent! ints!))))))




(defn round

  "Rounds a number."

  [^double x]

  (Math/round x))




(defn wrap-to-range

  "Like `fit-to-range` but the value is wrapped as if the range was cyclic.
  

   Ex. (wrap-to-range 0
                      10
                      -3)
       => 6"

  ([value]

   (wrap-to-range 0
                  1
                  value))


  ([max-value value]

   (wrap-to-range 0
                  max-value
                  value))


  ([min-value max-value value]

   (let [value' (rem (- value
                        min-value)
                     (- max-value
                        min-value))]
     (if (neg? value')
       (+ max-value
          1
          value')
       (+ min-value
          value')))))
