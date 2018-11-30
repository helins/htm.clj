(ns dvlopt.htm.encoder

  "Encoders mapping arbitrary values to SDRs.

   
   This is the entry point of any HTM system, inputs in various formats have to be translated into SDRs.
   It is a (fn encode [sdr input] sdr'). This namespace offers functions for creating encoders and modifying
   them.

   By default, encoder do not clear the SDR they work with so that they are more flexible. However, clearing
   is often needed in HTM systems so the user can refer to the `always-clear`.
  
  
   Based on :

     [1] Purdy, S. (2016). Encoding data for HTM systems. arXiv preprint arXiv:1602.05925."

  {:author "Adam Helinski"}

  (:require [dvlopt.htm           :as htm]
            [dvlopt.htm.sdr       :as htm.sdr]
            [dvlopt.htm.sdr.props :as htm.sdr.props]
            [dvlopt.htm.math      :as htm.math]))




;;;;;;;;;; Make encoders


(defn linear-encoder

  "Produces a linear encoder for scalar inputs.
  
   Inputs are normalized within `min-bound` and `max-bound` and out-of-range inputs are treated as
   extreme values.
  
  
   Generalization of [1] Section 3.1."

  ;; Based on implementation from Comportex.

  [capacity cardinality min-bound max-bound]

  (let [high-offset (dec cardinality)
        max-bound'  (- capacity
                       cardinality)]
    (fn encode-linear [sdr input]
      (let [normalized-input  (htm.math/min-max-normalization min-bound
                                                              max-bound
                                                              input)
            low-bit           (Math/round ^double (* normalized-input
                                                     max-bound'))
            high-bit          (+ low-bit
                                 high-offset)]
        (htm.sdr/set-bit-range sdr
                               low-bit
                               high-bit
                               true)))))



(defn cyclic-linear-encoder

  "Like `linear-encoder` but `min-bound` and `max-bound` are linked circularly (ie. the representation
   for the highest input overlaps with the lowest one).


   Inspired by [1] Section 4.1"

  ;; Based on implementation from Comportex.

  [capacity cardinality min-bound max-bound]

  (let [high-offset (dec cardinality)
        last-bit    (dec capacity)]
    (fn encode-cyclic-linear [sdr input]
      (let [normalized-input  (htm.math/min-max-normalization min-bound
                                                              max-bound
                                                              input)
            low-bit           (Math/round ^double (* normalized-input
                                                     last-bit))
            high-bit          (+ low-bit
                                 high-offset)
            sdr'              (htm.sdr/set-bit-range sdr
                                                     low-bit
                                                     (min high-bit
                                                          last-bit)
                                                     true)
            overflow          (- high-bit
                                 last-bit)]
        (if (pos? overflow)
          (htm.sdr/set-bit-range sdr'
                                 0
                                 (dec overflow)
                                 true)
          sdr')))))




(defn categorical-encoder

  "Representations of categories do not have overlapping active bits.

   A category is actually just an integer between 0 (inclusive) and `category-count` (exclusive).

   The cardinality is the result of the natural division of the capacity of the SDR and `category-count`.


   Based on [1] Section 4."

  [capacity category-count]

  (let [cardinality (quot capacity
                          category-count)
        high-offset (dec cardinality)]
    (fn encode-category [sdr input]
      (when (or (>= input
                   category-count)
                (< input
                   0))
        (throw (IllegalArgumentException. (format "Input category must be >= 0 and < %d"
                                                  category-count))))
      (let [low-bit     (* input
                           cardinality)
            high-bit    (+ low-bit
                           high-offset)]
        (htm.sdr/set-bit-range sdr
                               low-bit
                               high-bit
                               true)))))




;;;;;;;;;; Transform encoders


(defn always-clear

  "Transforms an encoder so that the given SDR will always be cleared."

  [encoder]

  (fn clear-sdr [sdr input]
    (encoder (htm.sdr/clear sdr)
             input)))
