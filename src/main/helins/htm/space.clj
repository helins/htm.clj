;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.htm.space

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

     active-inputs
      Sequence of `input`s active as a result of some stimulus.

     active-minicols
      Sequence of `minicol`s active as a result of `active-inputs`.

     center-input
       When relevant, a `minicol` might have a natural center in the input space.

     cnx, connection
       A connection is made when `perm` >= `cnx-threshold``.

     cnxs
       Vector where indices are `minicol`s and item are vectors of `input`s with established connection.

     cnx-threshold, connection-threshold
       Treshold for deciding if a `perm` is strong enough for establishing a `cnx`.

     flat-index
       Cf. `flat-index` in `helins.htm.grid` namespace.

     grid
       Cf. `grid` in `helins.htm.grid` glossary.

     grid-inputs
       `grid` describing the topology of the input space.

     grid-minicols
       `grid describing the topology of `minicols`.

     inhibition-radius
       Radius around a `minicol` within `grid-minicols` where inhibition happens during local inhibition.

     input
       Input bit represented by its `flat-index` within the `grid-inputs`.

     input-pool-mapping
       Vector where indices represent `input`s and items are 2-tuples where the first element is a `minicol` and the second one is the offset of the
       corresponding `input` in that `minicol`'s `pool`.

     minicol
       Modelisation of a neocortical mini-column represented by its `flat-index` within the `grid-minicols`.

     n-pool
       Size of a `pool`.

     overlap-score
       Number of currently `active-inputs` a minicol is currently connected to.

     overlap-scores
       Vector of `overlap-score`s where each index represent `minicol`;

     perm, permanence
       Value between 0 and 1 (inclusive) for deciding, in conjunction with a `cnx-threshold`, if a `cnx` is established.

     perms
       Vector of `perm`s layed out in the same order as their corresponding `pool`.

     perm-table
       Vector of `perms` where each index represents a `minicol`. `perms` are layed out in the same order as corresponding `pool`s.

     pool, potential-pool
       Set of `input`s a `minicol` can potentially connect to.

     pools, potential-pools
       Vector of `pool`s where each index represents a `minicol`.

     receptive-field
       Span of `input`s within `grid-inputs` a `minicol` is currently connected to.

     stimulus-threshold
       During inference, in order for a mini-column to even be considered potentially active, it must have an `overlap-score` of at least that much. During
       initialization, it is important to garantee that at least `stimulus-threshold` connections are randomly established for each mini-column otherwise
       they will never have the chance to compete. This parameter is a measure against noise and should be low. It could even be 0."

  {:author "Adam Helinski"}

  (:require [helins.htm.grid :as htm.grid]
            [helins.htm.math :as htm.math]))


;;;;;;;;;;


(defn local-pool

  "Creates a `pool` for the given `minicol` by sampling `input`s from a hypercube around the `minicol`'s corresponding `center-input`
   in `grid-inputs`."

  ;; TODO. Wrapping etc ?

  ([grid-inputs potential-radius n-pool grid-minicols minicol]

   (local-pool grid-inputs
               potential-radius
               n-pool
               grid-minicols
               minicol
               rand))


  ([grid-inputs potential-radius n-pool grid-minicols minicol rng]

   (htm.grid/sample-hypercube grid-inputs
                              (->> (htm.grid/f-index->coords grid-minicols
                                                             minicol)
                                   (htm.grid/relative-coords grid-inputs
                                                             grid-minicols)
                                   (htm.grid/hypercube* grid-inputs
                                                        potential-radius))
                              n-pool
                              rng)))




(defn global-pool

  "Creates a `pool` by sampling `input`s from the whole `grid-inputs`."

  ([grid-inputs n-pool]

   (global-pool grid-inputs
                n-pool
                 rand))


  ([grid-inputs n-pool rng]

   (htm.grid/sample-grid grid-inputs
                         n-pool
                         rng)))




(defn input-pool-mapping

  "Given `pools`, returns `perm-mapping`."

  [n-inputs pools]

  (reduce-kv (fn pool-view [ipools minicol pool]
               (reduce-kv (fn update-ipool [ipools' i input]
                            (update ipools'
                                    input
                                    conj
                                    [minicol i]))
                          ipools
                          pool))
             (vec (repeat n-inputs
                          []))
             pools))




(defn perms

  "Returns a sequence of `perms` size `n-pool` where `n-cnxs` values are above `cnx-threshold`. All values are within `cnx-threshold`
   +/- `cnx-delta`.
  
   Keep in mind `n-cnxs` should be >= `stimulus-threshold` later used otherwise the `minicol` associated with those intiial `perms`
   will not have a chance of ever be considered active."

  ;; TODO. Connections are often weighted by the distance between i-minicol and i-input, which requires the dimensionality
  ;; of the input-space as well as the dimensionality the mini-columnar topography. Typically, a mini-column is supposed
  ;; to be biased towards a natural center (ie. i-center-bit).

  ;; MAYBEDO. In the Python implementation, connections < some small threshold called 'synPermTrimThreshold' are zeroed
  ;; for "reducing memory requirements". Unclear how that would help here

  ([n-pool n-cnxs cnx-threshold cnx-delta]

   (perms n-pool
          n-cnxs
          cnx-threshold
          cnx-delta
          rand))


  ([n-pool n-cnxs cnx-threshold cnx-delta rng]

   (map (fn perm [connected?]
          (let [cnx-delta' (* cnx-delta
                              (rng))]
            (htm.math/fit-to-range 0
                                   1
                                   (+ cnx-threshold
                                      (if connected?
                                        cnx-delta'
                                        (- cnx-delta'))))))
        (htm.math/durstenfeld-shuffle rng
                                      (concat (repeat n-cnxs
                                                      true)
                                              (repeat (- n-pool
                                                         n-cnxs)
                                                      false))))))




(defn overlap-scores

  "Given `active-inputs`, returns `overlap-scores`."

  [cnx-threshold perm-table input-pool-mapping active-inputs]

  (reduce (fn each-input [overlap-scores active-input]
            (reduce (fn update-count [overlap-scores' [minicol perm-pos]]
                      (if (>= (get-in perm-table
                                      [minicol
                                       perm-pos])
                              cnx-threshold)
                        (update overlap-scores'
                                minicol
                                inc)
                        overlap-scores'))
                    overlap-scores
                    (get input-pool-mapping
                         active-input)))
          (vec (repeat (count perm-table)
                       0))
          active-inputs))




(defn active-duty-cycles

  ""

  [n-minicols]

  (vec (repeat n-minicols)
       0))




(defn update-duty-cycle

  ""

  [period duty-cycles updates]

  (mapv (fn with-update [duty-cycle updte]
          (/ (+ (* (dec period)
                   duty-cycle)
                updte)
             period))
        duty-cycles
        updates))



(defn update-active-duty-cycles

  ""

  [period active-minicols active-duty-cycles]

  (update-duty-cycle period
                     active-duty-cycles
                     (reduce (fn set-update [updates active-minicol]
                               (assoc updates
                                      active-minicol
                                      1))
                             (vec (repeat (count active-duty-cycles)
                                          0))
                             active-minicols)))




(defn boost-factors

  ""

  [n-minicols]

  (vec (repeat n-minicols
               1)))




(defn global-inhibition

  "Using `overlap-score`s of mini-columns, performs global inhibition by selecting `n-active` `i-minicols` with
   the best `overlap`."

  ;; TODO. Tie breaking, possiblity by using unstable sorting.
  ;; TODO. Stimulus threshold.

  [overlap-scores n-active]

  (take n-active
        (sort-by overlap-scores
                 (reify java.util.Comparator

                   (compare [_ x y]
                     (- (compare x
                                 y))))
                 (range (count overlap-scores)))))




(defn avg-inputs-minicols-ratio

  "Computes, on average, the number of mini-columns existing for every input bit.
  
   Needed for computing the `inhibition-radius`."

  [grid-inputs grid-minicols]

  (htm.math/mean (map /
                      grid-minicols
                      grid-inputs)))




(defn cnxs

  "Returns a vector where indices are `minicol`s and items are vectors of `input`s with established connections."

  [cnx-threshold pools perm-table]

  (mapv (fn filter-connected [pool perms]
          (filterv some?
                   (map (fn only-connected [input perm]
                          (when (>= perm
                                    cnx-threshold)
                            input))
                        pool
                        perms)))
        pools
        perm-table))




(defn receptive-field

  "Given a list of `input`s with established connections, computes the `receptive-field` of a `minicol`."

  [grid-inputs minicol-cnxs]

  (htm.grid/dim-span (htm.grid/dim-ranges grid-inputs
                                          (map (fn to-coords [fi-input]
                                                 (htm.grid/f-index->coords grid-inputs
                                                                           fi-input))
                                               cnxs))))




(defn inhibition-radius

  "Computes the `inhibition-radius` for local inhibition."

  [grid-inputs avg-inputs-minicols-ratio cnxs]

  (let [avg-receptive-field (htm.math/mean (map htm.math/mean
                                                (map (fn compute-rf [minicol-cnxs]
                                                       (receptive-field grid-inputs
                                                                        minicol-cnxs))
                                                     cnxs)))
        diameter            (* avg-receptive-field
                               avg-inputs-minicols-ratio)]
    (max (htm.math/round (/ (dec diameter)
                            2))
         1)))




(defn local-inhibition

  "Using `overlap-score`s of mini-columns, performs a local inhibition."

  ;; TODO. Tie breaking by favoring already active mini-columns.
  ;; TODO. Stimulus threshold.

  [grid-minicols n-active inhibition-radius overlap-scores]

  (reduce-kv (fn against-neighors [active-minicols minicol overlap-score]
               (let [neighborhood  (htm.grid/hypercube* grid-minicols
                                                        inhibition-radius
                                                        (htm.grid/f-index->coords grid-minicols
                                                                                  minicol))
                     ;; TODO. Not always true, depends if the hypercube is wrapping or not.
                     n-neighbors   (Math/pow inhibition-radius
                                             (count grid-minicols))]
                 (loop [neighbor         0
                        n-bigger-overlap 0]
                   (if (< neighbor
                          n-neighbors)
                     (let [n-bigger-overlap' (if (> (get overlap-scores
                                                         (htm.grid/f-index->coords-hypercube neighborhood
                                                                                             neighbor))
                                                    overlap-score)
                                               (inc n-bigger-overlap)
                                               n-bigger-overlap)]
                       (if (< n-bigger-overlap'
                              n-active)
                         (recur (inc neighbor)
                                n-bigger-overlap')
                         active-minicols))
                     (conj active-minicols
                           minicol)))))
             []
             overlap-scores))




(defn adapt-perms

  "Spatial learning, adapts permanences of `active-minicols`."

  [perm- perm+ n-inputs pools active-inputs perm-table active-minicols]

  (let [perm-updates (reduce (fn incrementing [perm-updates input]
                               (assoc perm-updates
                                      input
                                      perm+))
                             (vec (repeat (count n-inputs)
                                          (- perm-)))
                             active-inputs)]
    (reduce (fn update-minicol [perm-table' active-minicol]
              (update perm-table'
                      active-minicol
                      (fn update-perms [perms]
                        (mapv (fn update-perm [i perm]
                                (htm.math/fit-to-range 0
                                                       1
                                                       (+ perm
                                                          (get perm-updates
                                                               i))))
                              (range (count perms))
                              perms))))
            perm-table
            active-minicols)))
