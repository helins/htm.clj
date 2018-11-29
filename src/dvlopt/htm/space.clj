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
       Can be computed from an `index`, which would be a bit like folding a dimension into an N dimensional space.

     dimensions
       Vector defining an N dimensional space (eg. a 2D 32x64 grid is [32 64]).

     index
       The index of an item regardless of its dimensionality (eg. an input bit located at [4 10] in a 2D 32x64 grid would have an index of (+ (* 4 64) 10).
       It is like unfolding an N dimensional space into a single dimension.

     i-bit
       The `index` of an input bit in the input space.

     i-minicol
       The `index` of a mini column.

     input-connection
       Tuple [`i-bit` `connection`].

     n-bits
       Total number of input bits in the input space or a subset of the input space.

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


(defn normalize-coordinates

  "Normalizes `coordinates` within an N dimensional space defined by `dimensions`"

  [dimensions coordinates]

  (mapv (fn normalize-coordinate [capacity-dimension coordinate]
          (double (/ coordinate
                     capacity-dimension)))
        dimensions
        coordinates))




(defn denormalize-coordinates

  "Does the opposite of `normalize-coordinates` while rounding coordinates to integers."

  [dimensions normalized-coordinates]

  (mapv (fn denormalize-coordinate [capacity-dimension normalized-coordinate]
          (htm.util/round(* capacity-dimension
                            normalized-coordinate)))
        dimensions
        normalized-coordinates))




(defn coordinates->index

  "Computes the `index` of `coordinates` in an N dimensional space."

  [dimensions coordinates]

  (reduce-kv (fn add-dimension [index i-dimension capacity-dimension]
               (+ (* index
                     capacity-dimension)
                  (get coordinates
                       i-dimension)))
             0
             dimensions))




(defn index->coordinates

  "Computes the `coordinates` of an `index` in an N dimensional space."

  [dimensions index]

  (let [n-dimensions (count dimensions)
        coordinates  (int-array n-dimensions)]
    (aset-int coordinates
              0
              (reduce (fn compute-dimension [index' i-dimension]
                        (let [capacity-dimension (get dimensions
                                                      i-dimension)]
                          (aset-int coordinates
                                    i-dimension
                                    (rem index'
                                         capacity-dimension))
                          (/ index'
                             capacity-dimension)))
                      index
                      (range (dec n-dimensions)
                             0
                             -1)))
    (vec coordinates)))




(defn coord-center-input

  "Maps mini-column `coordinates` to the `coordinates` of an associated center input bit (ie. the natural center of a mini-column
   within the input space)."

  [dim-input dim-minicol coord-minicol]

  (denormalize-coordinates dim-input
                           (normalize-coordinates dim-minicol
                                                  coord-minicol)))




(defn i-center-input

  "Like `coord-center-input` but works with indexes."

  [dim-input dim-minicol i-minicol]

  (coordinates->index dim-input
                      (coord-center-input dim-input
                                          dim-minicol
                                          (index->coordinates dim-minicol
                                                              i-minicol))))




(defn hypercube

  "Returns a vector expressing a hypercube in an N dimensional space.
  
   Each item is a 2-tuple forming an inclusive interval in the corresponding dimension.
  
   The hypercube is truncated if it goes beyond the original N dimensional space unless `wrapping?` is set to
   true, in which case it wraps around to the other side."

  ;; TODO. Constrained hypercube.
  ;; MAYBEDO. Generalize to hyperrectangles, quite easy to do.

  ([dimensions radius coord-center]

   (hypercube false
              dimensions
              radius
              coord-center))


  ([wrapping? dimensions radius coord-center]

   (reduce-kv (fn i-dim-range [hypercube i-dimension capacity-dimension]
                (let [i-coordinate (get coord-center
                                        i-dimension)
                      left         (- i-coordinate
                                      radius)
                      right        (+ i-coordinate
                                      radius)]
                  (conj hypercube
                        (if wrapping?
                          [left
                           right]
                          [(max 0
                                left)
                           (min (dec capacity-dimension)
                                right)]))))
              []
              dimensions)))




(defn dim-hypercube

  "Transforms a `hypercube` into a well-formed N dimension space."

  [hypercube]

  (reduce (fn capacity-dimension [dim-hypercube' [min-coord max-coord]]
            (conj dim-hypercube'
                  (- (inc max-coord)
                     min-coord)))
          []
          hypercube))




(defn space-capacity

  "Computes the total capacity of an N dimensional space."

  [space]

  (reduce *
          space))






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
