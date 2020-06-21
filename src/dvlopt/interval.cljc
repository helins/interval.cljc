(ns dvlopt.interval

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
  ;;     https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core :as clj]
            [clojure.set  :as clj.set]))


;;;;;;;;;; Private


(declare ^:private -point<+)



(defn- -mark-segments

  ;;

  [value from-2 to tree [[[from-seg
                           to-seg
                           :as segment]
                          values]
                         & segments]]

  (if segment
    (cond
      (= to
         to-seg)        (assoc tree
                               segment
                               (conj values
                                     value))
      (-point<+ to
                to-seg) (-> tree
                            (dissoc segment)
                            (assoc [from-seg to] (conj values
                                                       value)
                                   [to to-seg]   values))
      :else             (recur value
                               to-seg
                               to
                               (assoc tree
                                      segment
                                      (conj values
                                            value))
                               segments))
    (assoc tree
           [from-2 to]
           #{value})))



(defn- -point<-

  ;; Handles nil as infinity.

  [a b]

  (or (nil? a)
      (and (some? b)
           (< a
              b))))



(defn- -point<+

  ;; Handles nil as infinity.

  [a b]

  (or (nil? b)
      (and (some? a)
           (< a
              b))))


;;;;;;;;;; Public API


(defn- -erase-value

  ""

  [tree segment values value]

  (let [values-2 (disj values
                       value)]
    (if (empty? values-2)
      (dissoc tree
              segment)
      (assoc tree
             segment
             values-2))))



(defn- -erase-segments

  ""

  [value to tree [[[from-seg
                    to-seg
                    :as segment]
                   values]
                  & segments]]

  (if segment
    (if (contains? values
                   value)
      (cond
        (= to
           to-seg)        (-erase-value tree
                                        segment
                                        values
                                        value)

        (-point<+ to
                  to-seg) (let [tree-2   (-> tree
                                             (dissoc segment)
                                             (assoc [to to-seg]
                                                    values))
                                values-2 (disj values
                                               value)]
                            (if (empty? values-2)
                              tree-2
                              (assoc tree-2
                                     [from-seg to]
                                     values-2)))
        :else             (recur value
                                 to
                                 (-erase-value tree
                                               segment
                                               values
                                               value)
                                 segments))
      (recur value
             to
             tree
             segments))
    tree))



(defn erase

  ""

  [tree from to value]

  (let [[[[from-seg
           to-seg
           :as segment]
          values]
         & segments]    (subseq tree
                                >= from)]
    (if (or (nil? segment)
            (and (some? from-seg)
                 (some? to)
                 (<= to
                     from-seg)))
      tree
      (if (contains? values
                     value)
        (if (or (= from
                   from-seg)
                (-point<- from
                          from-seg))
          (cond
            (= to
               to-seg)        (-erase-value tree
                                            segment
                                            values
                                            value)

            (-point<+ to
                      to-seg) (let [tree-2   (-> tree
                                                 (dissoc segment)
                                                 (assoc [to to-seg]
                                                        values))
                                    values-2 (disj values
                                                   value)]
                                (if (empty? values-2)
                                  tree-2
                                  (assoc tree-2
                                         [from-seg to]
                                         values-2)))
            :else             (-erase-segments value
                                               to
                                               (-erase-value tree
                                                             segment
                                                             values
                                                             value)
                                               segments))
          (let [tree-2   (-> tree
                             (dissoc segment)
                             (assoc [from-seg from]
                                    values))
                values-2 (disj values
                               value)]
            (cond
              (= to
                 to-seg)        (if (empty? values-2)
                                  tree-2
                                  (assoc tree-2
                                         [from to]
                                         values-2))
              (-point<+ to
                        to-seg) (let [tree-3 (assoc tree-2
                                                    [to to-seg]
                                                    values)]
                                  (if (empty? values-2)
                                    tree-3
                                    (assoc tree-3
                                           [from to]
                                           values-2)))
              :else             (-erase-segments value
                                                 to
                                                 (if (empty? values-2)
                                                   tree-2
                                                   (assoc tree-2
                                                          [from to-seg]
                                                          values-2))
                                                 segments))))
        (if (or (= to
                   to-seg)
                (-point<+ to
                          to-seg))
          tree
          (-erase-segments value
                           to
                           tree
                           segments))))))



(defn mark 

  ""

  ;; Following Allen's interval algebra, the first `if` tests the following relations:
  ;;
  ;;   X < Y
  ;;   X > Y
  ;;   X MEETS Y
  ;;   Y MEETS X
  ;;
  ;; The 3x3 `cond` forms test (in order):
  ;;
  ;;   X = Y
  ;;   X STARTS Y
  ;;   Y STARTS X
  ;;  
  ;;   Y FINISHES X
  ;;   X OVERLAPS Y
  ;;   Y DURING X
  ;;  
  ;;   X FINISHES Y
  ;;   X DURING Y
  ;;   Y OVERLAPS X

  ;; A bit fugly and handcrafted, but does the job efficiently as it minimizes looping and hitting
  ;; the sorted-map.

  [tree from to value]

  (let [[[[from-seg
           to-seg
           :as segment]
          values] 
         & segments]    (subseq tree
                                >= from)]
    (if (or (nil? segment)
            (and (some? from-seg)
                 (some? to)
                 (<= to
                     from-seg)))
      (assoc tree
             [from to]
             #{value})
      (cond
        (= from
           from-seg)        (cond
                              (= to
                                 to-seg)        (assoc tree
                                                       segment
                                                       (conj values
                                                             value))
                              (-point<+ to
                                        to-seg) (-> tree
                                                    (dissoc segment)
                                                    (assoc [from to]   (conj values
                                                                             value)
                                                           [to to-seg] values))
                              :else
                              (-mark-segments value
                                              to-seg
                                              to
                                              (assoc tree
                                                     segment
                                                     (conj values
                                                           value))
                                              segments))
        (-point<- from       
                  from-seg) (let [tree-2 (-> tree
                                             (dissoc segment)
                                             (assoc [from from-seg]
                                                    #{value}))]
                              (cond
                                (= to
                                   to-seg)        (assoc tree-2
                                                         segment
                                                         (conj values
                                                               value))
                                (-point<+ to
                                          to-seg) (assoc tree-2
                                                         [from-seg to] (conj values
                                                                             value)
                                                         [to to-seg]   values)
                                :else
                                (-mark-segments value
                                                to-seg
                                                to
                                                (assoc tree-2
                                                       segment
                                                       (conj values
                                                             value))
                                                segments)))
        :else              (let [tree-2 (-> tree
                                            (dissoc segment)
                                            (assoc [from-seg from]
                                                   values))]
                             (cond
                               (= to
                                  to-seg)        (assoc tree-2
                                                        [from to]
                                                        (conj values
                                                              value))
                               (-point<+ to
                                         to-seg) (assoc tree-2
                                                        [from to]   (conj values
                                                                          value)
                                                        [to to-seg] values)
                               :else
                               (-mark-segments value
                                               to-seg
                                               to
                                               (assoc tree-2
                                                      [from to-seg]
                                                      (conj values
                                                            value))
                                               segments)))))))



(defn lesser-than 

  ""

  [a b]

  (if (nil? a)
    true
    (if (nil? b)
      false
      (if (number? a)
        (if (number? b)
          (< a
             b)
          (if-some [b-2 (first b)]
            (< a
               b-2)
            false))
        (if-some [a-2 (second a)]
          (if (number? b)
            (<= a-2
                b)
            (if-some [b-2 (first b)]
              (<= a-2
                  b-2)
              false))
          false)))))



(defn tree

  ""

  []

  (sorted-map-by lesser-than))



(defn union

  ""

  [segments]

  (reduce (fn dedup-values [values [_segment values-segment]]
            (clj.set/union values
                           values-segment))
          #{}
          segments))
