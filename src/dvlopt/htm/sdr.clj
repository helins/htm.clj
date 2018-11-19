(ns dvlopt.htm.sdr

  "Sparse Distributed Representations.
  
   Based on :

     [1] Ahmad, S., & Hawkins, J. (2015). Properties of sparse distributed representations and their application to hierarchical
           temporal memory. arXiv preprint arXiv:1503.07469."

  {:author "Adam Helinski"}

  (:require [dvlopt.htm      :as htm]
            [dvlopt.htm.util :as htm.util]
            [dvlopt.void     :as void]))




;;;;;;;;;;


(defn sdr

  "Creates a new SDR.
  
   A map of options may be given :
  
     ::capacity
       The length of the binary vector."

  ([]

   (sdr nil))


  ([options]

   (vec (repeat (void/obtain ::capacity
                             options
                             htm/defaults)
                false))))




(defn active-bit?

  "Is the ith bit active in the given SDR ?"

  [sdr i]

  (get sdr
       i))




(defn capacity

  "What is the capacity of the given SDR ?"

  [sdr]

  (count sdr))




(defn cardinality

  "What is the number of active bits in the given SDR ?"

  [sdr]

  (count (filter true?
                 sdr)))




(defn sparsity

  "Sparsity if the cardinality of the SDR divided by its capacity."

  [sdr]

  (/ (cardinality sdr)
     (capacity sdr)))




(defn union

  "OR the given SDRs."

  [& sdrs]

  (when (some identity
              sdrs)
    (into []
          (map (fn check-bit [i]
                 (boolean (some (fn find-active-bit [sdr]
                                  (active-bit? sdr
                                               i))
                                sdrs)))
               (range (capacity (first sdrs)))))))




(defn overlap

  "Computes the set of bits active in both SDRs.

   Returns a set of indices."

  [sdr-1 sdr-2]

  (reduce (fn check-bit [overlapping-bits i]
            (if (and (active-bit? sdr-1
                                  i)
                     (active-bit? sdr-2
                                  i))
              (conj overlapping-bits
                    i)
              overlapping-bits))
          #{}
          (range (capacity sdr-1))))




(defn overlap-score

  "Computes the number of bits active in both SDRs."

  [sdr-1 sdr-2]

  (count (overlap sdr-1
                  sdr-2)))




(defn match?

  "Do those two SDRs match ?
  
   Not providing a required minimal overlap-score will test for a perfect match."

  ([sdr-1 sdr-2]

   (match? (cardinality sdr-1)
           sdr-1
           sdr-2))


  ([overlap-score sdr-1 sdr-2]

   (>= (dvlopt.htm.sdr/overlap-score sdr-1
                                     sdr-2)
       overlap-score)))




(defn count-patterns

  "Computes the number of unique patterns an SDR can produce.
  

   Cf. [1] Equation 1"

  [capacity cardinality]

  (htm.util/count-combinations capacity
                               cardinality))




(defn P-exact-match

  "Computes the propability of an exact match between two random SDRs.


   Cf. [1] Equation 2"

  [capacity cardinality]

  (/ 1
     (count-patterns capacity
                     cardinality)))




(defn count-matching-SDRs

  "For any SDR `x`, computes the number of similar random SDRs matching `x`.
  

   Cf. [1] Equation 3"

  ([capacity cardinality overlap-score]

   (count-matching-SDRs capacity
                        cardinality
                        cardinality
                        overlap-score))


  ([capacity cardinality-x cardinality-other overlap-score]

   (when (or (> overlap-score
                cardinality-x)
             (> overlap-score
                cardinality-other))
     (throw (IllegalArgumentException. "Overlap score must be <= cardinality")))
   (*' (htm.util/count-combinations cardinality-other
                                    overlap-score)
       (htm.util/count-combinations (- capacity
                                       cardinality-x)
                                    (- cardinality-other
                                       overlap-score)))))




(defn P-inexact-match

  "Computes the propability of an inexact match between two random SDRs (ie. false positive match).

   A false positive happens when two random SDRs match inexactly but not exactly.

   In order to be robust against noise, an inexact match is a lot more useful than an exact one. However, this also rises the
   probability of a false positive match and it is important to know how. The higher the capacity and the overlap
   score, the lower the probability.
  
  
   Cf. [1] Equation 4"

  ([capacity cardinality minimal-overlap-score]

   (P-inexact-match capacity
                    cardinality
                    cardinality
                    minimal-overlap-score))


  ([capacity cardinality-1 cardinality-2 minimal-overlap-score]

   (/ (reduce (fn numerator [sum overlap-score]
                (+' sum
                    (count-matching-SDRs capacity
                                         cardinality-1
                                         cardinality-2
                                         overlap-score)))
              0
              (range minimal-overlap-score
                     (inc cardinality-2)))
      (htm.util/count-combinations capacity
                                   cardinality-2))))




(defn P-approx-inexact-match

  "Approximates efficiently the result of `P-inexact-match`.

   Specially precise when `cardinality-2` > 7 and `overlap-score` > (/ `cardinality` 2)


   Cf. [1] Equation 5"

  ([capacity cardinality overlap-score]

   (P-approx-inexact-match capacity
                           cardinality
                           cardinality
                           overlap-score))


  ([capacity cardinality-1 cardinality-2 overlap-score]
   (/ (count-matching-SDRs capacity
                           cardinality-1
                           cardinality-2
                           overlap-score)
      (htm.util/count-combinations capacity
                                   cardinality-2))))
