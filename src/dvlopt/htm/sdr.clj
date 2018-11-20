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
                                                      i->active?))))
        )))
        



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

   #_(when (or (> overlap-score
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
