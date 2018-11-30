(ns dvlopt.htm.sdr

  "Create and act on Sparse Distributed Representations.


   Cf. `dvlopt.htm.sdr.props`"

  {:author "Adam Helinski"}

  (:require [dvlopt.htm           :as htm]
            [dvlopt.htm.sdr.props :as htm.sdr.props]
            [dvlopt.void          :as void])
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

  "What is the number of active bits in this SDR ?"

  [sdr]

  (count (active-bits sdr)))




(defn sparsity

  "What is the percentage of active bits in this SDR ?"

  [sdr]

  (htm.sdr.props/sparsity (capacity sdr)
                          (cardinality sdr)))




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
          (range (min (capacity sdr-1)
                      (capacity sdr-2)))))




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
