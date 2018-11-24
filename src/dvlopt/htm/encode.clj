(ns dvlopt.htm.encode

  "Encoders mapping arbitrary values to SDRs.

   
   This is the entry point of any HTM system, inputs in various formats have to be translated into SDRs.
  
  
   Based on :

     [1] Purdy, S. (2016). Encoding data for HTM systems. arXiv preprint arXiv:1602.05925."

  {:author "Adam Helinski"}

  (:require [dvlopt.htm           :as htm]
            [dvlopt.htm.sdr       :as htm.sdr]
            [dvlopt.htm.sdr.props :as htm.sdr.props]
            [dvlopt.htm.util      :as htm.util]))




;;;;;;;;;; Linear encoding


(defn linear-encoding

  "Linear encoding for scalar inputs.
  
   Inputs are normalized within `min-bound` and `max-bound` and out-of-range inputs are treated as
   extreme values.
  
  
   Generalization of [1] Section 3.1."

  ;; Based on implementation from Comportex.

  [sparsity min-bound max-bound sdr input]

  (let [sdr'              (htm.sdr/clear sdr)
        capacity          (htm.sdr/capacity sdr')
        cardinality       (htm.sdr.props/cardinality capacity
                                                     sparsity)
        normalized-input  (htm.util/normalize min-bound
                                              max-bound
                                              input)
        low-bit           (Math/round ^double (* normalized-input
                                                 (- capacity
                                                    cardinality)))
        high-bit          (+ low-bit
                             (dec cardinality))]
    (htm.sdr/set-bit-range sdr'
                           low-bit
                           high-bit
                           true)))




(defn cyclic-linear-encoding

  "Like `linear-encoding` but `min-bound` and `max-bound` are linked circularly (ie. the representation
   for the highest input overlaps with the lowest one).


   Inspired by [1] Section 4.1"

  ;; Based on implementation from Comportex.

  [sparsity min-bound max-bound sdr input]

  (let [sdr'              (htm.sdr/clear sdr)
        capacity          (htm.sdr/capacity sdr')
        cardinality       (htm.sdr.props/cardinality capacity
                                                     sparsity)
        normalized-input  (htm.util/normalize min-bound
                                              max-bound
                                              input)
        last-bit          (dec capacity)
        low-bit           (Math/round ^double (* normalized-input
                                                 last-bit))
        high-bit          (+ low-bit
                             (dec cardinality))
        sdr'2             (htm.sdr/set-bit-range sdr'
                                                 low-bit
                                                 (min high-bit
                                                      last-bit)
                                                 true)
        overflow          (- high-bit
                             last-bit)]
    (if (pos? overflow)
      (htm.sdr/set-bit-range sdr'2
                             0
                             (dec overflow)
                             true)
      sdr'2)))




(defn categorical-encoding

  "Representations of categories do not have overlapping active bits.

   A category is actually just an integer between 0 (inclusive) and `category-count` (exclusive).

   The cardinality is the result of the natural division of the capacity of the SDR and `category-count`.


   Based on [1] Section 4."

  [category-count sdr category]

  (let [sdr'        (htm.sdr/clear sdr)
        cardinality (quot (htm.sdr/capacity sdr)
                          category-count)
        low-bit     (* category
                       cardinality)
        high-bit    (+ low-bit
                       (dec cardinality))]
    (htm.sdr/set-bit-range sdr'
                           low-bit
                           high-bit
                           true)))
