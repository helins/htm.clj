(ns dvlopt.htm.sdr

  "Sparse Distributed Representations.
  
   Based on :

     [1] Ahmad, S., & Hawkins, J. (2015). Properties of sparse distributed representations and their application to hierarchical
           temporal memory. arXiv preprint arXiv:1503.07469."

  {:author "Adam Helinski"}

  (:require [dvlopt.htm      :as htm]
            [dvlopt.htm.util :as htm.util]
            [dvlopt.void     :as void])
  (:import clojure.lang.Seqable))




;;;;;;;;;; Protocol


(defprotocol ISDR

  "Basic functions for implementing a SDR."

  (active-bit? [sdr i]

    "Is the `i`iest bit active in this SDR ?")


  (capacity [sdr]

    "How many bits is this SDR composed of ?")


  (serialize [sdr]

    "Transforms this SDR into a byte array.")


  (set-bit [sdr i active?]

    "Sets the `i`th bit in this SDR.")


  (set-bits [sdr i->active?]

    "Efficiently sets the required bits in this SDR following a map of index -> boolean."))




;;;;;;;;;; Creating SDRs


(defn immutable-SDR-from

  "Creates a new immutable SDR from a sequence."

  [sq]

  (let [sdr       (into []
                        (map boolean)
                        sq)
        capacity' (count sdr)]
    (reify

      Seqable

        (seq [_]
          (seq sdr))


      ISDR

        (active-bit? [_ i]
          (get sdr
               i))
  
        (capacity [_]
          capacity')

        (serialize [_]
          (into-array Boolean/TYPE
                      sdr))

        (set-bit [_ i active?]
          (immutable-SDR-from (assoc sdr
                                     i
                                     (boolean active?))))

        (set-bits [_ i->active?]
          (immutable-SDR-from (persistent! (reduce-kv (fn update-bit [sdr' i active?]
                                                        (assoc! sdr'
                                                                i
                                                                active?))
                                                      (transient sdr)
                                                      i->active?)))))))
        



(defn immutable-SDR

  "Creates a new immutable SDR.
  

   A map of options may be given :
  
     ::capacity
       The length of the binary vector."

  ([]

   (immutable-SDR nil))


  ([options]

   (immutable-SDR-from (repeat (void/obtain ::capacity
                                            options
                                            htm/defaults)
                               false))))




(defn deserialize-immutable-SDR

  "Deserialize an immutable SDR."

  [ba]

  (immutable-SDR-from (map nat-int?
                           ba)))




;;;;;;;;;; Functions manipulating SDRs based on the basic functions defined in the protocol


(defn active-bits

  "Lazily computes a sorted list containing indices of active bits from this SDR."

  ([sdr]

   (filter (fn test-unit [i]
             (active-bit? sdr
                          i))
           (range (capacity sdr)))))




(defn cardinality

  "What is the number of active units in the given SDR ?"

  [sdr]

  (count (active-bits sdr)))




(defn sparsity

  "Sparsity if the cardinality of this SDR divided by its capacity."

  [sdr]

  (/ (cardinality sdr)
     (capacity sdr)))




(defn union

  "Lazily computes a sequence representing the union of all the fiven SDRs.
  
  
   Cf. [1] Section G"

  [& sdrs]

  (when (some identity
              sdrs)
    (map (fn check-bit [i]
           (boolean (some (fn find-active-bit [sdr]
                            (active-bit? sdr
                                         i))
                          sdrs)))
         (range (capacity (first sdrs))))))




(defn overlap

  "Lazy computes a sorted list of bits active in both SDRs."

  [sdr-1 sdr-2]

  (filter (fn check-bit [i]
            (and (active-bit? sdr-1
                              i)
                 (active-bit? sdr-2
                              i)))
          (range (capacity sdr-1))))




(defn overlap-score

  "Computes the number of bits active in both SDRs."

  [sdr-1 sdr-2]

  (count (overlap sdr-1
                  sdr-2)))




(defn match-inexactly?

  "Do those two SDRs have at least `overlap-score` active bits in common ?"

  [overlap-score sdr-1 sdr-2]

  (>= (dvlopt.htm.sdr/overlap-score sdr-1
                                    sdr-2)
      overlap-score))




(defn match-exactly?

  "Do those two SDRs match exactly ?"

  [sdr-1 sdr-2]

  (match-inexactly? (cardinality sdr-1)
                    sdr-1
                    sdr-2))




;;;;;;;;;; Functions computing some useful properties


(defn count-patterns

  "Computes the number of unique patterns an SDR can produce.
  

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

   Patterns can be subsamples where `cardinality-subsample` < `cardinality`.
  

   Cf. [1] Equation 3, 6"

  ([capacity overlap-score cardinality]

   (count-inexact-patterns capacity
                           overlap-score
                           cardinality
                           cardinality))


  ([capacity overlap-score cardinality cardinality-subsample]

   #_(when (or (> overlap-score
                cardinality)
             (> overlap-score
                cardinality-subsample))
     (throw (IllegalArgumentException. "Overlap score must be <= cardinality")))

   (*' (count-patterns cardinality-subsample
                       overlap-score)
       (count-patterns (- capacity
                          cardinality-subsample)
                       (- cardinality
                          overlap-score)))))




(defn P-inexact-match

  "Computes the propability of an inexact match between two random SDRs either sharing the same properties or one being
   a subsample (`cardinality-subsample` < `cardinality`).
 
   In order to be robust against noise, an inexact match is a lot more useful than an exact one. The probability of an
   inexact match represent the probability of a false positive. If both SDRs have the same cardinality, that would be
   the occurence of an inexact match despite the fact they do not match exactly. When subsampling, that would be an
   inexact match despite the fact the subsample is not subsampled from the tested SDR.
 
 
   The higher the capacity and the overlap score, the lower probability.
  
  
   Cf. [1] Equation 4, 7"

  ([capacity min-overlap-score cardinality]

   (P-inexact-match capacity
                    min-overlap-score
                    cardinality
                    cardinality))


  ([capacity min-overlap-score cardinality cardinality-subsample]

   (/ (reduce (fn numerator [sum overlap-score]
                (+' sum
                    (count-inexact-patterns capacity
                                            overlap-score
                                            cardinality
                                            cardinality-subsample)))
              0
              (range min-overlap-score
                     (inc cardinality-subsample)))
      (count-patterns capacity
                      cardinality))))




(defn P-approx-inexact-match

  "Approximates efficiently the result of `P-inexact-match`.

   Specially precise when cardinality > 7 and overlap-score` > (/ cardinality` 2).


   Cf. [1] Equation 5"

  ([capacity overlap-score cardinality]

   (P-approx-inexact-match capacity
                           overlap-score
                           cardinality
                           cardinality))


  ([capacity overlap-score cardinality cardinality-subsample]
   (/ (count-inexact-patterns capacity
                              overlap-score
                              cardinality
                              cardinality-subsample)
      (count-patterns capacity
                      cardinality-subsample))))
