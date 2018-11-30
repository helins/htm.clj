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


     center-input
       When relevant, a minicolumn might have a natural center in the input space.

     connection
       A connection is made when `permanence` >= `connection-threshold`.

     connection-delta
       During connections initialization, radius around the `connection-threshold` for randomly choosing an initial `permanence`.

     connection-density
       Percentage of `connection`s above `connection-threshold`.

     connection-threshold
       Treshold for deciding if a `permanence` is strong enough for establishing a `connection`.

     coordinate, coord
       Integer expression the location of an item in a dimension, such as the location of an input bit.

     coordinates, coords
       Vector of `coordinate`s expressing a location in an N dimensional space.
       Can be computed from an index, which would be a bit like folding a single dimension into an N dimensional space.

     coords-center-input
       Coordinate of a `center-input`

     dimensions
       Vector defining an N dimensional space (eg. a 2D 32x64 grid is [32 64]).

     index
       The index of an item regardless of its dimensionality (eg. an input bit located at [4 10] in a 2D 32x64 grid would have an index of (+ (* 4 64) 10).
       It is like unfolding an N dimensional space into a single dimension.

     i-input
       The `index` of an input bit in the input space.

     i-minicol
       The `index` of a mini column.

     n-inputs
       Total number of input bits in the input space or a subset of the input space.

     n-minicols
       Total number of mini-columns.

     overlap-score
       Number of currently active input bits a mini-column is currently connected to.

     permanence, perm
       Value between 0 and 1 (inclusive) for deciding, in conjunction with a `connection-threshold`, if a `connection` is established.

     potential-density
       During initialization, the percentage of input bits that are sampled for creating a `potential-pool`.

     potential-pool
       The set of `i-input`s a `i-minicol` can potentially connect to.

     stimulus-threshold
       During inference, in order for a mini-column to even be considered potentially active, it must have an `overlap-score` of at least that much. During
       initialization, it is important to garantee that at least `stimulus-threshold` connections are randomly established for each mini-column otherwise
       they will never have the chance to compete. This parameter is a measure against noise and should be low. It could even be 0.
  "

  {:author "Adam Helinski"}

  (:require [dvlopt.htm.math      :as htm.math]
            [dvlopt.htm.sdr.props :as htm.sdr.props]))




;;;;;;;;;;


(defn normalize-coordinates

  "Normalizes `coordinates` within an N dimensional space defined by `dimensions`"

  [dimensions coordinates]

  (mapv (fn normalize-coord [capacity-dimension coordinate]
          (double (/ coordinate
                     capacity-dimension)))
        dimensions
        coordinates))




(defn denormalize-coordinates

  "Does the opposite of `normalize-coordinates` while rounding coordinates to integers."

  [dimensions normalized-coordinates]

  (mapv (fn denormalize-coordinate [capacity-dimension normalized-coordinate]
          (htm.math/round (* capacity-dimension
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




(defn wrap-coordinate

  "Wraps a coordinate to a dimension."

  [capacity-dimension coordinate]

  (let [i-last      (dec capacity-dimension)
        coordinate' (rem coordinate
                         capacity-dimension)]
    (if (neg? coordinate')
      (+ capacity-dimension
         coordinate')
      (let [overflow (- coordinate'
                        i-last)]
        (if (pos? overflow)
          overflow
          coordinate')))))




(defn wrap-coordinates

  "Wraps coordinates to an N dimensional space."

  [dim-input coordinates]

  (mapv wrap-coordinate
        dim-input
        coordinates))




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
                  (+ (htm.math/abs min-coord)
                     max-coord
                     1)))
          []
          hypercube))




(defn space-capacity

  "Computes the total capacity of an N dimensional space."

  [space]

  (reduce *
          space))




(defn index->hypercube-coordinates

  "Computes the `coordinates` of an index within a hypercube."

  ([hypercube index]

   (index->hypercube-coordinates hypercube
                                 (dim-hypercube hypercube)
                                 index))


  ([hypercube dim-hypercube index]

   (mapv (fn ??? [[min-dim max-dim] coordinate]
           ;; Wrap or not to wrap ?
           (+ coordinate
              min-dim))
         hypercube
         (index->coordinates dim-hypercube
                             index))))




(defn potential-hypercube

  "For `i-minicol`, computes the hypercube in the input space where potential connections should be sampled."

  [dim-input potential-radius dim-minicol i-minicol]

  (hypercube dim-input
             potential-radius
             (coord-center-input dim-input
                                 dim-minicol
                                 (index->coordinates dim-minicol
                                                     i-minicol))))




(defn sample-inputs

  "Samples `i-input` from the whole input space."

  ([dim-input n-sample]

   (sample-inputs dim-input
                  n-sample
                  rand))


  ([dim-input n-sample rng]

   (htm.math/reservoir-sample-ints rng
                                   (space-capacity dim-input)
                                   n-sample)))




(defn sample-potential-hypercube

  "Sample `n-sample` input bits from a potential hypercube of inputs."

  ([dim-input potential-hypercube n-sample]

   (sample-potential-hypercube dim-input
                               potential-hypercube
                               n-sample
                               rand))

  ([dim-input potential-hypercube n-sample rng]

   (let [dim-hypercube' (dim-hypercube potential-hypercube)]
     (mapv (fn space-index [index]
             (->> (index->hypercube-coordinates potential-hypercube
                                                dim-hypercube'
                                                index)
                  (wrap-coordinates dim-input)
                  (coordinates->index dim-input)))
           (sample-inputs dim-hypercube'
                          n-sample
                          rng)))))




(defn local-potential-pool

  "Samples `n-potential-connections` for `i-minicol` from a potential hypercube around its natural center in the
   input space."

  ([dim-input dim-minicol i-minicol potential-radius n-potential-connections]

   (local-potential-pool dim-input
                         dim-minicol
                         i-minicol
                         potential-radius
                         n-potential-connections
                         rand))


  ([dim-input dim-minicol i-minicol potential-radius n-potential-connections rng]

   (sample-potential-hypercube dim-input
                               (hypercube dim-input
                                          potential-radius
                                          (coord-center-input dim-input
                                                              dim-minicol
                                                              (index->coordinates dim-minicol
                                                                                  i-minicol)))
                               n-potential-connections
                               rng)))




(defn init-permanences

  "Returns a sequence of `n-potential` permanence values. Around `connection-density` percent should be above `connection-threshold`.
   Those permanences will span between `connection-threshold` +/- `connection-delta`.
  
   Keep in mind `n-connections` should be >= the stimulus threshold later used otherwise the mini-column associated with those
   initial permanences will not have a chance of ever be considered active."

  ;; In order to symplify the work, the returned sequences contains the connected permanences first and disconnected last.


  ;; TODO. Connections are often weighted by the distance between i-minicol and i-input, which requires the dimensionality
  ;; of the input-space as well as the dimensionality the mini-columnar topography. Typically, a mini-column is supposed
  ;; to be biased towards a natural center (ie. i-center-bit).

  ;; MAYBEDO. In the Python implementation, connections < some small threshold called 'synPermTrimThreshold' are zeroed
  ;; for "reducing memory requirements". Unclear how that would help here

  ([n-potential n-connections connection-threshold connection-delta]

   (init-permanences n-potential
                     n-connections
                     connection-threshold
                     connection-delta
                     rand))


  ([n-potential n-connections connection-threshold connection-delta rng]

   (map (fn permanence [connected?]
          (let [connection-delta' (* connection-delta
                                     (rng))]
            (htm.math/fit-to-range (+ connection-threshold
                                      (if connected?
                                        connection-delta'
                                        (- connection-delta'))))))
        (concat (repeat n-connections
                        true)
                (repeat (- n-potential
                           n-connections)
                        false)))))
