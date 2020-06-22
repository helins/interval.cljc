(ns dvlopt.interval

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
  ;;     https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core :as clj]
            [clojure.set  :as clj.set]))


;;;;;;;;;; Private


(defn- -overlapping?

  ;;

  [to from]

  (or (nil? to)
      (nil? from)
      (> to
         from)))


(defn- -erase-value

  ;;

  [tree segment values value]

  (let [values-2 (disj values
                       value)]
    (if (empty? values-2)
      (dissoc tree
              segment)
      (assoc tree
             segment
             values-2))))



(defn- -point<-

  ;; Handles nil as infinity.

  [a b]

  (if (nil? a)
    (some? b)
    (and (some? b)
         (< a
            b))))



(defn- -point<=-

  ;;

  [a b]

  (or (= a
         b)
      (nil? a)
      (and (some? b)
           (<= a
               b))))



(defn- -point<+

  ;; Handles nil as infinity.

  [a b]

  (if (nil? b)
    (some? a)
    (and (some? a)
         (< a
            b))))



(defn- -point<=+

  ;;

  [a b]

  (or (= a
         b)
      (nil? b)
      (and (some? a)
           (<= a
               b))))



(defn- -restore-values

  ;;

  [tree segment values value]

  (let [values-2 (disj values
                       value)]
    (if (empty? values-2)
      tree
      (assoc tree
             segment
             values-2))))


;;;;;;;;;; Public API


(defn- -erase-segments

  ;; Used by [[erase]].
  ;;
  ;; Behaves very much like [[erase]] but optimizes the algorithm is not dealing with the first
  ;; segment anymore. Is pretty much a copy/paste, but the recursion and all the destructuring
  ;; make it not worth the trouble abstracting that away.

  [value to tree [[[from-seg
                    to-seg
                    :as segment]
                   values]
                  & segments]]

  (if (and segment
           (-overlapping? to
                          from-seg))
    (if (contains? values
                   value)
      (if (-point<+ to
                    to-seg)
        (-restore-values (-> tree
                             (dissoc segment)
                             (assoc [to to-seg]
                                    values))
                         [from-seg to]
                         values
                         value)
        (let [tree-2 (-erase-value tree
                                   segment
                                   values
                                   value)]
          (if (= to
                 to-seg)
            tree-2
            (recur value
                   to
                   tree-2
                   segments))))
      (if (-point<=+ to
                     to-seg)
        tree
        (recur value
               to
               tree
               segments)))
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
    (if (and segment
             (-overlapping? to
                            from-seg))
      (if (contains? values
                     value)
        (if (-point<=- from
                       from-seg)
          (if (-point<+ to
                        to-seg)
            (-restore-values (-> tree
                                 (dissoc segment)
                                 (assoc [to to-seg]
                                        values))
                             [from-seg to]
                             values
                             value)
            (let [tree-2 (-erase-value tree
                                       segment
                                       values
                                       value)]
              (if (= to
                     to-seg)
                tree-2
                (-erase-segments value
                                 to
                                 tree-2
                                 segments))))
          (let [tree-2 (-> tree
                           (dissoc segment)
                           (assoc [from-seg from]
                                  values))]
            (if (-point<+ to
                          to-seg)
              (-restore-values (assoc tree-2
                                      [to to-seg]
                                      values)
                               [from to]
                               values
                               value)
              (let [tree-3 (-restore-values tree-2
                                            [from to-seg]
                                            values
                                            value)]
                (if (= to
                       to-seg)
                  tree-3
                  (-erase-segments value
                                   to
                                   tree-3
                                   segments))))))
        (if (-point<=+ to
                       to-seg)
          tree
          (-erase-segments value
                           to
                           tree
                           segments)))
      tree)))



(defn mark 

  ""

  ;; Following Allen's interval algebra, the first `if` tests the following relations:
  ;;
  ;;   X < Y
  ;;   X > Y
  ;;   X MEETS Y
  ;;   Y MEETS X
  ;;
  ;; The 3 `cond` forms with nested `if`s test (in order):
  ;;
  ;;   X STARTS Y
  ;;   X = Y
  ;;   Y STARTS X
  ;;  
  ;;   X OVERLAPS Y
  ;;   Y FINISHES X
  ;;   Y DURING X
  ;;  
  ;;   X DURING Y
  ;;   X FINISHES Y
  ;;   Y OVERLAPS X

  ;; A bit fugly and handcrafted, but does the job efficiently as it minimizes looping and hitting
  ;; the sorted-map.

  [tree from to value]

  (loop [from-2          from
         [[[from-seg
            to-seg
            :as segment]
           values] 
          & segments]    (subseq tree
                                 >= from)
         tree-2          tree]
    (if (and segment
             (-overlapping? to
                            from-seg))
      (cond
        (= from-2
           from-seg)        (if (-point<+ to
                                          to-seg)
                              (-> tree-2
                                  (dissoc segment)
                                  (assoc [from-2 to] (conj values
                                                           value)
                                         [to to-seg] values))
                              (let [tree-3 (assoc tree-2
                                                  segment
                                                  (conj values
                                                        value))]
                                (if (= to
                                       to-seg)
                                  tree-3
                                  (recur to-seg
                                         segments
                                         tree-3))))
        (-point<- from       
                  from-seg) (let [tree-3 (-> tree-2
                                             (dissoc segment)
                                             (assoc [from-2 from-seg]
                                                    #{value}))]
                              (if (-point<+ to
                                            to-seg)
                                (assoc tree-3
                                       [from-seg to] (conj values
                                                           value)
                                       [to to-seg]   values)
                                (let [tree-4 (assoc tree-3
                                                    segment
                                                    (conj values
                                                          value))]
                                  (if (= to
                                         to-seg)
                                    tree-4
                                    (recur to-seg
                                           segments
                                           tree-4)))))
        :else              (let [tree-3 (-> tree-2
                                            (dissoc segment)
                                            (assoc [from-seg from-2]
                                                   values))]
                             (if (-point<+ to
                                           to-seg)
                               (assoc tree-3
                                      [from-2 to] (conj values
                                                        value)
                                      [to to-seg] values)
                               (let [tree-4 (assoc tree-3
                                                   [from-2 to-seg]
                                                   (conj values
                                                         value))]
                                 (if (= to
                                        to-seg)
                                   tree-4
                                   (recur to-seg
                                          segments
                                          tree-4))))))
      (assoc tree-2
             [from-2 to]
             #{value}))))



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
