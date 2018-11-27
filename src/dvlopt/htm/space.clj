(ns dvlopt.htm.space

  "Spatial pooling.
  
  
   Based on :

     Byrne, F. (2015). Encoding reality: Prediction-assisted cortical learning algorithm in hierarchical temporal memory. arXiv preprint arXiv:1509.08255.

     Cui, Y., Ahmad, S., & Hawkins, J. (2017). The HTM spatial pooler—a neocortical algorithm for online sparse distributed coding. Frontiers in computational
       neuroscience, 11, 111.

     Hawkins, J. et al. 2016. Biological and Machine Intelligence. Release 0.4. Accessed at http://numenta.com/biological-and-machine-intelligence/.

     Leake, M., Xia, L., Rocki, K., & Imaino, W. (2015, January). Effect of Spatial Pooler Initialization on Column Activity in Hierarchical Temporal Memory.
       In AAAI (pp. 4176-4177).

     Mnatzaganian, J., Fokoué, E., & Kudithipudi, D. (2017). A mathematical formalization of hierarchical temporal memory’s spatial pooler. Frontiers in Robotics
       and AI, 3, 81.
     
     Pietroń, M., Wielgosz, M., & Wiatr, K. (2016, October). Formal analysis of HTM spatial pooler performance under predefined operation conditions. In International
       Joint Conference on Rough Sets (pp. 396-405). Springer, Cham.
  
  

   Glossary :


     connection
       Value between 0 and 1 expressing the strengh of a synaptic connection.

     connection-delta
       During connections initialization, radius around the `connection-threshold` for randomly choosing an initial `connection`.

     connection-density
       Percentage of `connection`s above `connection-threshold`.

     connection-threshold
       Treshold for deciding if a `connection` is strong enough for the synapse to be fully connected.

     coordinates
       Vector expressing the location of an item, such as the location of an input-bit in a 2D input space.

     index
       The index of an item regardless of its dimensionality (eg. an input bit located at [4 10] in a 2D 32 x 64 grid would have an index of (+ (* 4 64) 10).

     i-bit
       The `index` of an input bit in the input space.

     i-minicol
       The `index` of a mini column.

     input-connection
       Tuple [`i-bit` `connection`].

     n-bits
       Total number of input bits in the input space.

     n-minicols
       Total number of mini-columns.

     overlap-score
       Number of currently active input bits a mini-column is currently connected to.

     potential-density
       During initialization, the percentage of input bits that are sampled for creating a `potential-pool`.

     potential-pool
       The set of `i-bit`s a `i-minicol` can potentially connect to.

     stimulus-threshold
       During inference, in order for a mini-column to even be considered potentially active, it must have an `overlap-score` of at least that much. During
       initialization, it is important to garantee that at least `stimulus-threshold` connections are randomly established for each mini-column otherwise
       they will never have the chance to compete. This parameter is a measure against noise and should be low. For practical use, it could even be 0.
  "

  {:author "Adam Helinski"}

  (:require [dvlopt.htm.sdr.props :as htm.sdr.props]
            [dvlopt.htm.util      :as htm.util]))




;;;;;;;;;;


(defn potential-pool

  "Returns a vector of `i-minicol` -> `potential-pool`."

  ;; TODO. The potential radius delimits a hypercube in the input space around a center i-bits where the i-bits are sampled
  ;; for a mini-column. For now, i-bits are sampled from the whole input space.

  ([n-minicols n-bits potential-density]

   (potential-pool n-minicols
                   n-bits
                   potential-density
                   rand))


  ([n-minicols n-bits potential-density rng]

   (let [n-bits-per-minicol (htm.util/round (* n-bits
                                               potential-density))
         i-bits             (int-array (range n-bits))]
     (mapv (fn sample-i-bits [_]
             (vec (htm.util/sample-ints rng
                                        n-bits-per-minicol
                                        i-bits)))
           (range n-minicols)))))




(defn init-connections

  "Initializes the connection in a `potential-pool`.
  
   Returns a vector of `i-minicol` -> `input-connections`."

  ;; TODO. Connections are often weighted by the distance between i-minicol and i-bit, which requires the dimensionality
  ;; of the input-space as well as the dimensionality the mini-columnar topography. Typically, a mini-column is supposed
  ;; to be biased towards a natural center (ie. center i-bit).

  ;; TODO. Does the RNG really need to be called twice ? Might be expensive.

  ;; MAYBEDO. In the Python implementation, connections < some small threshold called 'synPermTrimThreshold' are zeroed
  ;; for "reducing memory requirements". Unclear how that would help here

  ([potential-pool connection-density connection-threshold connection-delta]

   (init-connections potential-pool
                     connection-density
                     connection-threshold
                     connection-delta
                     rand))


  ([potential-pool connection-density connection-threshold connection-delta rng]

   (mapv (fn set-connections [potential-i-bits]
           (mapv (fn set-connection [potential-i-bit]
                   (let [connected? (< (rng)
                                       connection-density)
                         x          (rng)
                         connection (htm.util/constrain-number (+ connection-threshold
                                                                  (* connection-delta
                                                                     (if connected?
                                                                       x
                                                                       (- x)))))]
                     [potential-i-bit
                      connection]))
                 potential-i-bits))
         potential-pool)))




(defn adjust-to-stimulus-threshold

  "For each mini-column, raises the input connections until that mini-column has at least `stimulus-threshold` established
   connections."

  [connection-threshold stimulus-threshold connection-increment i-minicol->input-connections]

  (if (zero? stimulus-threshold)
    i-minicol->input-connections
    (mapv (fn handlec-connections [input-connections]
            (let [n-connected (count (filter (fn connected? [[_ connection]]
                                               (>= connection
                                                   connection-threshold))
                                             input-connections))]
              (if (>= n-connected
                      stimulus-threshold)
                (vec input-connections)
                (recur (map (fn raise-connections [[i-bit connection :as tuple]]
                              (if (>= connection
                                      connection-threshold)
                                tuple
                                [i-bit
                                 (htm.util/constrain-number (+ connection
                                                               connection-increment))]))
                            input-connections)))))
          i-minicol->input-connections)))
