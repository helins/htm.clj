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


  (clear [sdr]

    "Turns off all bits in this SDR.")


  (serialize [sdr]

    "Transforms this SDR into a byte array.")


  (set-bit [sdr i active?]

    "Sets the `i`th bit in this SDR.")


  (set-bit-range [sdr i j active?]

    "Sets bits from `i` to `j` (both inclusive) in this SDR.")


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

        (clear [_]
          (immutable-SDR-from (repeat capacity'
                                      false)))

        (serialize [_]
          (into-array Boolean/TYPE
                      sdr))

        (set-bit [_ i active?]
          (immutable-SDR-from (assoc sdr
                                     ^long i
                                     (boolean active?))))

        (set-bit-range [_ i j active?]
          (immutable-SDR-from (persistent! (reduce (fn update-bit-in-range [sdr' ^long i']
                                                     (assoc! sdr'
                                                             i'
                                                             active?))
                                                   (transient sdr)
                                                   (range i
                                                          (inc j))))))

        (set-bits [_ i->active?]
          (immutable-SDR-from (persistent! (reduce-kv (fn update-bit [sdr' ^long i active?]
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

   (filter (fn test-bit [i]
             (active-bit? sdr
                          i))
           (range (capacity sdr)))))




(defn cardinality

  "What is the number of active units in the given SDR ?"

  [sdr]

  (count (active-bits sdr)))




(defn sparsity

  "Sparsity if the cardinality of this SDR divided by its capacity."

  ([sdr]

   (sparsity (capacity sdr)
             (cardinality sdr)))


  ([capacity cardinality]

   (/ cardinality
      capacity)))




(defn union

  "Lazily computes a sequence representing the union of all the given SDRs.
  
  
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




(defn overlapping-bits

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

  (count (overlapping-bits sdr-1
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

   Specially precise when cardinality > 7 and overlap-score` > (/ cardinality` 2).


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
