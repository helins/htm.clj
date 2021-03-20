;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.htm.grid

  "Regular cartesian grids.
  
  
   Glossary
   
     
     capacity
       How many `element`s fit on a dimension, a portion of a `grid`, or a whole `grid`.

     coordinate, coord
       Integer expression the unique location of an `element` in a dimension.

     coordinates, coords
       Vector of `coordinate`s expressing a unique location in a `grid`.

     dimension, dim
       Always described by its capacity.     

     element
       Member of a grid, always has unique `coordinates`.

     flat-index, f-index
       Any `coordinates` can be translated into a unique and corresponding 'f-index` between 0 (inclusive) and `capacity` (exclusive).
       For instance, in a 2D 32x64 grid, the element with coordinates [4 10] would have a `f-index` of (+ (* 4 64) 10).
       It is like unfolding an N dimensional space into a single dimension.

     grid
       Regular Cartesian grid partitioning a finite N dimensional space into a topology of `element`s with integer `coordinates`.
       Described by a vector of `dimension`s. A 2D 32x64 grid would be [32 64].

     hypercube
       Vector of 2-tuples [`coordinate` `capacity`], one for every dimension, expressing a hypercube in a `grid`. 
       A `coordinate` can be outside of the capacity of the corresponding `grid` dimension. Depending on the context, the hypercube would be truncated
       or wrapped (cf. `wrapping`).

     n-dims
       Number of dimensions.

     normal-coords
       `coordinates` normalized to fit between 0 and 1 (inclusive).

     random-number-generator, rng
       No-arg function returning a random value between 0 (inclusive) and 1 (exclusive).

     relative-coords
       `coordinates` in one `grid` always have relative `coordinates` in another `grid`.
       For instance, `coordinates` [1 1] in a [4 4] `grid` can be translated to `coordinates` [3 3] in a [10 10] `grid` which, proportionally, represents the same location.

     relative-f-index
       Just like `relative-coords`, but for a `flat-index`.

     wrapping
       Dimensions can behave in a cyclic manner. For instance, a `coordinate` of -2 on a dimension of `capacity` 6 would effectively be 4.
  "
  
  {:author "Adam Helinski"}

  (:require [helins.htm.math :as htm.math]))


;;;;;;;;;; Coordinates and flat-indexes


(defn normalize-coords

  "Normalizes `coordinates` to be between 0 and 1."

  [grid coords]

  (mapv (fn normalize-coord [dim coord]
          (double (/ coord
                     (dec dim))))
        grid
        coords))




(defn denormalize-coords

  "Does the opposite of `normalize-coords`."

  [grid normal-coords]

  (mapv (fn denormalize-coord [dim normal-coord]
          (htm.math/round (* dim
                             normal-coord)))
        grid
        normal-coords))




(defn coords->f-index

  "Translates `coordinates` to the corresponding `flat-index` in the given `grid`."

  [grid coords]

  (reduce-kv (fn unfold-dimension [f-index i-dim dim]
               (+ (* f-index
                     dim)
                  (get coords
                       i-dim)))
             0
             grid))




(defn f-index->coords

  "Translates a `flat-index` to the corresponding `coordinates` in the given `grid`."

  [grid f-index]

  (let [n-dims (count grid)
        coords (int-array n-dims)]
    (aset-int coords
              0
              (reduce (fn compute-dimension [f-index' i-dim]
                        (let [dim (get grid
                                       i-dim)]
                          (aset-int coords
                                    i-dim
                                    (rem f-index'
                                         dim))
                          (/ f-index'
                             dim)))
                      f-index
                      (range (dec n-dims)
                             0
                             -1)))
    (vec coords)))




(defn relative-coords

  "Maps `coordinates` in `grid-2` to relative `coordinates` in `grid-1`."

  [grid-1 grid-2 coords-2]

  (->> coords-2
       (normalize-coords grid-2)
       (denormalize-coords grid-1)))




(defn relative-f-index

  "Like `relative-coords` but works with `flat-index`es."

  [grid-1 grid-2 f-index-2]

  (->> f-index-2
       (f-index->coords grid-2)
       (relative-coords grid-1
                        grid-2)
       (coords->f-index grid-1)))




(defn wrap-coords

  "Cf. `wrapping` in glossary."

  [grid coords]

  (mapv (fn wrap-coord [dim coord]
          (htm.math/wrap-to-range 0
                                  dim
                                  coord))
        grid
        coords))




;;;;;;;;;; Grids


(defn grid-capacity

  "Computes the capacity of a grid (ie. how many elements it represents)."

  [grid]

  (reduce *
          grid))




;;;;;;;;;; Hypercubes


(defn hypercube*

  "Builds a `hypercube` in a `grid` from `coordinates` expressing its center."

  ;; TODO. Constrained hypercube.
  ;; MAYBEDO. Generalize to hyperrectangles, quite easy to do.

  ([grid radius coords-center]

   (reduce-kv (fn next-dim [hypercube i-dim dim]
                (let [coord    (- (get coords-center
                                       i-dim)
                                  radius)
                      capacity (inc (* 2
                                       radius))]
                  (conj hypercube
                        [coord
                         capacity])))
              []
              grid)))




(defn hypercube->grid

  "Transforms a `hypercube` into a `grid`."

  [hypercube]

  (reduce (fn next-dim [grid [_ capacity]]
            (conj grid
                  capacity))
          []
          hypercube))




(defn f-index->coords-hypercube

  "Computes the `coordinates` of a `flat-index` within a `hypercube`."

  [hypercube f-index]

  (mapv (fn ??? [[coord _capacity] grid-coord]
          ;; TODO. Wrap or not to wrap ?
          (+ coord
             grid-coord))
        hypercube
        (f-index->coords (hypercube->grid hypercube)
                         f-index)))




(comment

  ;; TODO. Useful ?

  (defn potential-hypercube

    "For `i-minicol`, computes the hypercube in the input space where potential connections should be sampled."

    [dim-input potential-radius dim-minicol i-minicol]

    (hypercube dim-input
               potential-radius
               (coord-center-input dim-input
                                   dim-minicol
                                   (index->coordinates dim-minicol
                                                       i-minicol)))))




;;;;;;;;;; Samplings elements from grids and hypercubes


(defn sample-grid

  "Samples `n-sample` random `element`s from a grid."

  ([grid n-sample]

   (sample-grid grid
                n-sample
                rand))


  ([grid n-sample rng]

   (htm.math/reservoir-sample-ints rng
                                   0
                                   (grid-capacity grid)
                                   n-sample)))




(defn sample-hypercube

  "Sample `n-sample` input bits from a `hypercube` within a `grid`."

  ([grid hypercube n-sample]

    (sample-hypercube grid
                      hypercube
                      n-sample
                      rand))


  ([grid hypercube n-sample rng]

   (mapv (fn grid-f-index [f-index]
           ;; TODO. Proper wrapping etc.
           (->> (f-index->coords-hypercube hypercube
                                           f-index)             
                (wrap-coords grid)
                (coords->f-index grid)))
         (sample-grid (hypercube->grid hypercube)
                      n-sample
                      rng))))




;;;;;;;;;; Misc


(defn dim-ranges

  "Given a sequence of `coordinates` in a `grid`, returns a vector of [min-coord max-coord] for every dimension."

  [grid seq-coords]

  (reduce (fn update-dim-ranges  [min-maxs coords]
            (mapv (fn by-dim [[min-coord max-coord] coord]
                    [(min min-coord
                          coord)
                     (max max-coord
                          coord)])

                  min-maxs
                  coords))
          (mapv (fn init-dim-range [coord]
                  [coord
                   coord])
                (first seq-coords))
          (rest seq-coords)))




(defn dim-span

  "Given `dim-ranges`, computes the span for every dimension."

  [dim-ranges]

  (mapv (fn by-dim [[min-coord max-coord]]
          (- max-coord
             min-coord))
        dim-ranges))
