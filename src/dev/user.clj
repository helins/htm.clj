(ns user

  "For daydreaming at the REPL."

  (:require [helins.htm           :as htm]
            [helins.htm.grid      :as htm.grid]
            [helins.htm.encoder   :as htm.encoder]
            [helins.htm.math      :as htm.math]
            [helins.htm.sdr       :as htm.sdr]
            [helins.htm.sdr.props :as htm.sdr.props]
            [helins.htm.space     :as htm.space]))


;;;;;;;;;;


(comment
  

  (def pools
       (mapv (fn [_minicol]
               (htm.space/global-pool [4]
                                      3))
             (range 10)))


  (def input-pool-mapping
       (htm.space/input-pool-mapping 4
                                     pools))


  (def perm-table
       (mapv (fn [_]
               (vec (htm.space/perms 3
                                     2
                                     0.5
                                     0.1)))
             (range 10)))


  (def overlap-scores
       (htm.space/overlap-scores 0.5
                                 perm-table
                                 input-pool-mapping
                                 [0 2]))


  (def cnxs
       (htm.space/cnxs 0.5
                       pools
                       perm-table))

  



  )
