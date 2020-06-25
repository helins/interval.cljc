(ns dvlopt.interval.set

  ""

  {:author "Adam Helinski"}

  (:require [dvlopt.interval.util :as interval.util])
  (:refer-clojure :exclude [empty]))


;;;;;;;;;; Marking


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


;;;;;;;;;; Erasing


(defn- -erase-rest

  ""

  [iset to [[from-seg
             to-seg
             :as segment]
            & segments]]

  (if (or (nil? segment)
          (interval.util/disjoint? to
                                   from-seg))
    iset
    (let [iset-2 (disj iset
                       segment)]
      (cond
        (= to
           to-seg)                     iset-2
        (interval.util/point<+ to
                               to-seg) (conj iset-2
                                             [to to-seg])
        :else                          (recur iset-2
                                              to
                                              segments)))))



(defn erase

  ""

  [iset from to]

  (loop [[[from-seg
           to-seg
           :as segment]
          & segments]   (subseq iset
                                >= from)
         iset-2        iset]
    (if (or (nil? segment)
            (interval.util/disjoint? to
                                     from-seg))
      iset-2
      (let [iset-3 (disj iset
                         segment)
            iset-4 (if (interval.util/point<=+ from
                                               from-seg)
                     iset-3
                     (conj iset-3
                           [from-seg from]))]
        (cond
          (= to
             to-seg)                     iset-4
          (interval.util/point<+ to
                                 to-seg) (conj iset-4
                                               [to to-seg])
          :else                          (-erase-rest iset-4
                                                      to
                                                      segments))))))


;;;;;;;;;;


(def empty

  ""

  (sorted-set-by interval.util/cmp))
