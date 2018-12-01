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


     coords-center-input
       Coordinate of a `center-input`

     dimensions
       Vector defining an N dimensional space (eg. a 2D 32x64 grid is [32 64]).


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

  ;; TODO. Update glossary now that there is a namespace for grids.

  {:author "Adam Helinski"}

  (:require [dvlopt.htm.grid      :as htm.grid]
            [dvlopt.htm.math      :as htm.math]
            [dvlopt.htm.sdr.props :as htm.sdr.props]))




;;;;;;;;;;


(defn local-potential-pool

  "Samples `n-potential-connections` for `i-minicol` from a potential hypercube around its natural center in the
   input space."

  ;; TODO. Wrapping etc ?

  ([grid-inputs potential-radius n-potential-connections grid-minicols i-minicol]

   (local-potential-pool grid-inputs
                         potential-radius
                         n-potential-connections
                         grid-minicols
                         i-minicol
                         rand))


  ([grid-inputs potential-radius n-potential-connections grid-minicols i-minicol rng]

   (htm.grid/sample-hypercube grid-inputs
                              (htm.grid/hypercube* grid-inputs
                                                   potential-radius
                                                   (htm.grid/relative-coords grid-inputs
                                                                             grid-minicols
                                                                             (htm.grid/f-index->coords grid-minicols
                                                                                                       i-minicol)))
                              n-potential-connections
                              rng)))




(defn global-potential-pool

  "Samples `n-potential-connections´ from the whole input space."

  ([grid-inputs n-potential-connections]

   (global-potential-pool grid-inputs
                          n-potential-connections
                          rand))


  ([grid-inputs n-potential-connections rng]

   (htm.grid/sample-grid grid-inputs
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
