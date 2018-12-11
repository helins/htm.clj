(ns user

  "For daydreaming in the repl."

  (:require [clojure.spec.alpha              :as s]
            [clojure.spec.gen.alpha          :as gen]
            [clojure.spec.test.alpha         :as st]
            [clojure.test.check.clojure-test :as tt]
            [clojure.test.check.generators   :as tgen]
            [clojure.test.check.properties   :as tprop]
            [clojure.test                    :as t]
            [criterium.core                  :as ct]
            [dvlopt.htm                      :as htm]
            [dvlopt.htm.grid                 :as htm.grid]
            [dvlopt.htm.encoder              :as htm.encoder]
            [dvlopt.htm.math                 :as htm.math]
            [dvlopt.htm.sdr                  :as htm.sdr]
            [dvlopt.htm.sdr.props            :as htm.sdr.props]
            [dvlopt.htm.space                :as htm.space]
            [dvlopt.void                     :as void]))




;;;;;;;;;;


(comment
  

  (def pools
       (mapv (fn [minicol]
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
