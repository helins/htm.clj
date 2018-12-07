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




(defn random-connections

  "Returns a random vector of `n-potential` connections with `n-connections`."

  ([n-potential n-connections]

   (random-connections n-potential
                       n-connections
                       rand))


  ([n-potential n-connections rng]

   (htm.math/durstenfeld-shuffle rng
                                 (concat (repeat n-connections
                                                 true)
                                         (repeat (- n-potential
                                                    n-connections)
                                                 false)))))




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
        (random-connections n-potential
                            n-connections
                            rng))))




(defn global-mapping

  "Returns a vector of where each index is a `i-input` point to a vector of [i-minicol permanence]."

  [grid-inputs grid-minicols n-potential n-connections connection-threshold connection-delta rng]

  (reduce (fn init-minicol [i-inputs i-minicol]
            (reduce (fn add-minicol [i-inputs' [i-input permanence]]
                      (assoc i-inputs'
                             i-input
                             (conj (get i-inputs'
                                        i-input)
                                   [i-minicol permanence])))
                    i-inputs
                    (partition 2
                               (interleave (global-potential-pool grid-inputs
                                                                  n-potential
                                                                  rng)
                                           (init-permanences n-potential
                                                             n-connections
                                                             connection-threshold
                                                             connection-delta
                                                             rng)))))
          (vec (repeat (htm.grid/grid-capacity grid-inputs)
                       []))
          (range (htm.grid/grid-capacity grid-minicols))))




(defn overlap-score

  "Given a sequence of `fi-inputs`, computes the `overlap-score` for every mini-column."

  [connection-threshold global-mapping fi-inputs]

  (reduce (fn compute-fi-input [i-minicol->overlap-score fi-input]
            (reduce (fn update-minicol [i-minicol->overlap-score' [i-minicol perm]]
                      (if (>= perm
                              connection-threshold)
                        (assoc i-minicol->overlap-score'
                               i-minicol
                               (inc (get i-minicol->overlap-score'
                                         i-minicol
                                         0)))
                        i-minicol->overlap-score'))
                    i-minicol->overlap-score
                    (get global-mapping
                         fi-input)))
          {}
          fi-inputs))




(defn overlap-score-2

  "Given a sequence of `fi-inputs`, computes the `overlap-score` for every mini-column."

  [connection-threshold n-minicols global-mapping fi-inputs]

  (reduce (fn compute-fi-input [i-minicol->overlap-score fi-input]
            (reduce (fn update-minicol [^ints i-minicol->overlap-score' [i-minicol perm]]
                      (when (>= perm
                                connection-threshold)
                        (aset-int i-minicol->overlap-score'
                                  i-minicol
                                  (inc (aget i-minicol->overlap-score'
                                             i-minicol))))
                      i-minicol->overlap-score')
                    i-minicol->overlap-score
                    (get global-mapping
                         fi-input)))
          (int-array n-minicols)
          fi-inputs))




(defn overlap-score-3

  "Given a sequence of `fi-inputs`, computes the `overlap-score` for every mini-column."

  [connection-threshold n-minicols global-mapping fi-inputs]

  (let [i-minicol->overlap-score (int-array n-minicols)]
    (dotimes [i (count fi-inputs)]
      (let [minicol-perms (get global-mapping
                               i)]
        (dotimes [j (count minicol-perms)]
          (let [[i-minicol
                 perm]     (get minicol-perms
                                j)]
            (when (>= perm
                      connection-threshold)
              (aset-int i-minicol->overlap-score
                        i-minicol
                        (inc (aget i-minicol->overlap-score
                                   i-minicol))))))))
    i-minicol->overlap-score))




(defn overlap-score-4

  "Given a sequence of `fi-inputs`, computes the `overlap-score` for every mini-column."

  [connection-threshold n-minicols global-mapping fi-inputs]

  (let [i-minicol->overlap-score (int-array n-minicols)]
    (doseq [fi-input fi-inputs]
      (doseq [[i-minicol
               perm]     (get global-mapping
                              fi-input)]
        (when (>= perm
                  connection-threshold)
          (aset-int i-minicol->overlap-score
                    i-minicol
                    (inc (aget i-minicol->overlap-score
                               i-minicol))))))
    i-minicol->overlap-score))




(defn global-inhibition

  "Using `overlap-score`s of mini-columns, performs global inhibition by selecting `n-active` `i-minicols` with
   the best `overlap`."

  ;; TODO. Tie breaking, possiblity by using unstable sorting.
  ;; TODO. Stimulus threshold.

  [i-minicol->overlap n-active]

  (take n-active
        (sort-by (fn by-overlap [[_i-minicol overlap-score]]
                   overlap-score)
                 (reify java.util.Comparator

                   (compare [_ x y]
                     (- (compare x
                                 y))))
                 i-minicol->overlap)))




(defn avg-inputs-minicols-ratio

  "Computes, on average, the number of mini-columns existing for every input bit."

  [grid-inputs grid-minicols]

  (htm.math/mean (map /
                      grid-minicols
                      grid-inputs)))




(defn fi-minicol->connections

  "Returns a vector of `fi-minicol` -> vector of `fi-inputs` with established connections."

  [n-minicols connection-threshold mapping]

  (reduce-kv (fn by-fi-input [fi-minicol->fi-inputs fi-input minicol-permanences]
               (reduce (fn by-fi-minicol [fi-minicol->fi-inputs' [fi-minicol perm]]
                         (if (>= perm
                                 connection-threshold)
                           (update fi-minicol->fi-inputs'
                                   fi-minicol
                                   conj
                                   fi-input)
                           fi-minicol->fi-inputs'))
                       fi-minicol->fi-inputs
                       minicol-permanences))
             (vec (repeat n-minicols
                          []))
             mapping))




(defn receptive-field

  "Given a list of `fi-input` of established connections, computes the receptive field of a mini-column."

  [grid-inputs connections]

  (htm.grid/dim-span (htm.grid/dim-ranges grid-inputs
                                          (map (fn to-coords [fi-input]
                                                 (htm.grid/f-index->coords grid-inputs
                                                                           fi-input))
                                               connections))))




(defn inhibition-radius

  "Computes the inhibition radius for local inhibition."

  [grid-inputs avg-inputs-minicols-ratio fi-minicol->connections]

  (let [avg-receptive-field (htm.math/mean (map htm.math/mean
                                                (map (fn compute-rf [connections]
                                                       (receptive-field grid-inputs
                                                                        connections))
                                                     fi-minicol->connections)))
        diameter            (* avg-receptive-field
                               avg-inputs-minicols-ratio)]
    (max (htm.math/round (/ (dec diameter)
                            2))
         1)))




(defn local-inhibition

  "Using `overlap-score`s of mini-columns, performs a local inhibition."

  ;; TODO. Tie breaking by favoring already active mini-columns.
  ;; TODO. Stimulus threshold.

  [grid-minicols n-active inhibition-radius fi-minicol->overlap-score]

  (reduce-kv (fn ??? [active-minicols fi-minicol overlap-score]
               (let [neighborhood  (htm.grid/hypercube* grid-minicols
                                                        inhibition-radius
                                                        (htm.grid/f-index->coords grid-minicols
                                                                                  fi-minicol))
                     ;; TODO. Not always true, depends if the hypercube is wrapping or not.
                     n-neighbors   (Math/pow inhibition-radius
                                             (count grid-minicols))]
                 (loop [fi-neighbor      0
                        n-bigger-overlap 0]
                   (if (< fi-neighbor
                          n-neighbors)
                     (let [n-bigger-overlap' (if (> (get fi-minicol->overlap-score
                                                         (htm.grid/f-index->coords-hypercube neighborhood
                                                                                             fi-neighbor))
                                                    overlap-score)
                                               (inc n-bigger-overlap)
                                               n-bigger-overlap)]
                       (if (< n-bigger-overlap'
                              n-active)
                         (recur (inc fi-neighbor)
                                n-bigger-overlap')
                         active-minicols))
                     (conj active-minicols
                           fi-minicol)))))
             []
             fi-minicol->overlap-score))
