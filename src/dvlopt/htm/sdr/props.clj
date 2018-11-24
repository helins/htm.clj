(ns dvlopt.htm.sdr.props

  "Properties of Sparse Distributed Representations (SDRs).


   It is a good practise to learn this namespace as SDRs are omnipresent in HTM. It introduces definitions and basic concepts.
   The `dvlopt.htm.sdr` namespace is about creating and acting on concrete SDRs whereas this namespace is about the theory.
  

   Based on :

     [1] Ahmad, S., & Hawkins, J. (2015). Properties of sparse distributed representations and their application to hierarchical
           temporal memory. arXiv preprint arXiv:1503.07469."

  {:author "Adam Helinski"}

  (:require [dvlopt.htm.util :as htm.util]))




;;;;;;;;;; Functions computing some useful properties


(defn cardinality

  "The cardinality of an SDR is the number of active bits. It can be computed from the capacity and
   the required sparsity."

  [capacity sparsity]

  (Math/round ^double (* capacity
                         (min 1
                              sparsity))))




(defn sparsity

  "The sparsity of an SDR is related to its cardinality as it is the percentage of active bits."

  [capacity cardinality]

  (/ (min cardinality
          capacity)
     capacity))




(defn count-patterns

  "Computes the number of unique patterns an SDR can produce given a fixed cardinality.
  

   Cf. [1] Equation 1"

  [capacity cardinality]

  (htm.util/count-combinations capacity
                               cardinality))




(defn P-exact-match

  "Computes the propability of an exact match between two random SDRs with the same properties.


   Cf. [1] Equation 2"

  [capacity cardinality]

  (/ 1
     (count-patterns capacity
                     cardinality)))




(defn count-inexact-patterns

  "Computes the number of patterns matching an SDR for exactly `overlap-score` active bits.
  
   They can have different cardinalities which is useful in the context of subsampling and unions.


   Generalization of [1] Equation 3, 6, and 14."

  ([capacity overlap-score cardinality]

   (count-inexact-patterns capacity
                           overlap-score
                           cardinality
                           cardinality))


  ([capacity overlap-score cardinality-1 cardinality-2]

   (*' (count-patterns cardinality-1
                       overlap-score)
       (count-patterns (- capacity
                          cardinality-1)
                       (- cardinality-2
                          overlap-score)))))




(defn P-inexact-match

  "Computes the propability of an inexact match between two random SDRs (at least `min-overlap-score` active bits in common).
   
   They can have different cardinalities which is useful in the context of subsampling and unions.
 
   In order to be robust against noise, an inexact match is a lot more useful than an exact one. The probability of an
   inexact match represent the probability of a false positive. The higher the capacity and the overlap score, the lower
   probability.

   Linear with respect to (min cardinality-1 cardinality-2).
  
  
   Generalization of [1] Equation 4, 7, and 15"

  ([capacity min-overlap-score cardinality]

   (P-inexact-match capacity
                    min-overlap-score
                    cardinality
                    cardinality))


  ([capacity min-overlap-score cardinality-1 cardinality-2]

   (/ (transduce (map (fn count-inexact-patterns' [overlap-score]
                        (count-inexact-patterns capacity
                                                overlap-score
                                                cardinality-1
                                                cardinality-2)))
                 +'
                 (let [exclusive-max-overlap-score (inc (min cardinality-1
                                                             cardinality-2))]
                   (range min-overlap-score
                          exclusive-max-overlap-score)))
      (count-patterns capacity
                      cardinality-2))))




(defn P-approx-inexact-match

  "Approximates efficiently the result of `P-inexact-match`.

   Specially precise when the smallest cardinality > 7 and overlap-score > (cardinality / 2).


   Cf. [1] Equation 5"

  ([capacity overlap-score cardinality]

   (P-approx-inexact-match capacity
                           overlap-score
                           cardinality
                           cardinality))


  ([capacity overlap-score cardinality-1 cardinality-2]
   (/ (count-inexact-patterns capacity
                              overlap-score
                              cardinality-1
                              cardinality-2)
      (count-patterns capacity
                      cardinality-2))))




(defn P-inactive-union-bit

  "Computes the probability that a given bit is inactive in a union of patterns.


   Cf. [1] Equation 12"

  [pattern-count capacity cardinality]

  (Math/pow (- 1
               (sparsity capacity
                         cardinality))
            pattern-count))




(defn P-active-union-bit

  "Opposite of `P-inactive-union-bit`."

  [pattern-count capacity cardinality]

  (- 1
     (P-inactive-union-bit pattern-count
                           capacity
                           cardinality)))




(defn P-exact-union-match

  "Computes the probability of a pattern exactly matching a pattern within a union of patterns.


   Cf. [1] Equation 13"

  [pattern-count capacity cardinality-2]

  (Math/pow (P-active-union-bit pattern-count
                                capacity
                                cardinality-2)
            cardinality-2))




(defn expected-union-cardinality

  "Computes the expected cardinality of a union of patterns.
  

   Cf. [1] Section G and H"

  [pattern-count capacity cardinality-2]

  (* capacity
     (P-active-union-bit pattern-count
                         capacity
                         cardinality-2)))
