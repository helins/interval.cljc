;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.interval.set

  ""

  {:author "Adam Helinski"}

  (:require [helins.interval.util :as interval.util])
  (:refer-clojure :exclude [empty]))


;;;;;;;;;; Marking


(defn- -mark-rest

  ;;

  [iset from to [[from-seg
                  to-seg
                  :as segment]
                 & segments]]

  (if (and segment
           (interval.util/point<=+ from-seg
                                   to))
    (if (interval.util/point<=+ to
                                to-seg)
      (-> iset
          (disj segment)
          (conj [from to-seg]))
      (recur (disj iset
                   segment)
             from
             to
             segments))
    (conj iset
          [from to])))




(defn mark

  ""

  [iset from to]

  (let [[from-left
         to-left
         :as segment-left] (first (rsubseq iset
                                           >= nil
                                           <  from))
        segments           (subseq iset
                                   >= from)]
    (if (some-> to-left
                (= from))
      (-mark-rest (disj iset
                        segment-left)
                  from-left
                  to
                  segments)
      (if-some [[from-seg
                 to-seg
                 :as segment] (first segments)]
        (let [from-2 (when (and (some? from)
                                (some? from-seg))
                       (min from
                            from-seg))]
          (if (interval.util/point<=+ to
                                      to-seg)
            (-> iset
                (disj segment)
                (conj [from-2 to-seg]))
            (-mark-rest (disj iset
                              segment)
                        from-2
                        to
                        (rest segments))))
        (conj iset
              [from to])))))


;;;;;;;;;; Erasing


(defn- -erase-rest

  ;;

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
