(ns dvlopt.interval.set

  ""

  {:author "Adam Helinski"}

  (:require [dvlopt.interval.util :as interval.util])
  (:refer-clojure :exclude [empty]))


;;;;;;;;;;


(defn mark

  ""

  [iset from to]

  (let [[from-left
         to-left
         :as segment-left] (first (rsubseq iset
                                           >= nil
                                           <  from))
        [from-2
         iset-2]           (if (some-> to-left
                                       (= from))
                             [from-left
                              (disj iset
                                    segment-left)]
                             [from
                              iset])]
    (loop [iset-3         iset-2
           [[from-seg
             to-seg
             :as segment]
            & segments]   (subseq iset-2
                                  >= from)]
      (if (and segment
               (interval.util/point<=+ from-seg
                                       to))
        (if (interval.util/point<=+ to
                                    to-seg)
          (-> iset-3
              (disj segment)
              (conj [from-2 to-seg]))
          (recur (disj iset-3
                       segment)
                 segments))
        (conj iset-3
              [from-2 to])))))


;;;;;;;;;;


(def empty

  ""

  (sorted-set-by interval.util/cmp))
