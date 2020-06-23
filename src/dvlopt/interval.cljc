(ns dvlopt.interval

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
  ;;     https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core :as clj]
            [clojure.set  :as clj.set]))


;;;;;;;;;; Private


(defn- -disjoint?

  ""

  [to from]

  (and (some? to)
       (some? from)
       (< to
          from)))


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




(defn- -mark-join

  ""

  [tree from to values value segments]

  (let [[[from-next
          to-next
          :as segment-next]
         values-next]       (first segments)
        values-2            (conj values
                                  value)]
    (if (and values-next
             (= to
                from-next)
             (= values-next
                values-2))
      (-> tree
          (dissoc [from to]       ;; TODO. Unnecessary when (not= from-2 from-seg) (cf. [[mark]])
                  segment-next)
          (assoc [from to-next]
                 values-2))
      (assoc tree
             [from to]
             values-2))))




(defn- -mark-segments

  ""

  ;; A bit fugly and handcrafted, but does the job efficiently as it minimizes looping and hitting
  ;; the sorted-map, while preserving from unnecessary fragmentation.

  [tree from-2 to value [[[from-seg
                           to-seg
                           :as segment]
                          values] 
                         & segments]]

  (if (or (nil? segment)
          (-disjoint? to
                      from-seg))
    (assoc tree
           [from-2 to]
           #{value})
    (if (contains? values
                   value)
      (if (-point<- from-2
                    from-seg)
        (let [tree-2 (if (= (count values)
                            1)
                       (-> tree
                           (dissoc segment)
                           (assoc [from-2 to-seg]
                                  values))
                       (assoc tree
                              [from-2 from-seg]
                              #{value}))]
          (if (-point<=+ to
                         to-seg)
            tree-2
            (recur tree-2
                   to-seg
                   to
                   value
                   segments)))
        (if (-point<=+ to
                       to-seg)
          tree
          (recur tree
                 to-seg
                 to
                 value
                 segments)))
      (cond
        (= to
           from-seg) (assoc tree
                            [from-2 to]
                            #{value})
        (= from-2
           from-seg) (if (-point<+ to
                                   to-seg)
                       (-> tree
                           (dissoc segment)
                           (assoc [from-2 to] (conj values
                                                    value)
                                  [to to-seg] values))
                       (if (= to
                              to-seg)
                         (-mark-join tree
                                     from-seg
                                     to-seg
                                     values
                                     value
                                     segments)
                         (recur (assoc tree
                                       segment
                                       (conj values
                                             value))
                                to-seg
                                to
                                value
                                segments)))
        :else         (let [tree-2 (-> tree
                                      (dissoc segment)
                                      (assoc [from-2 from-seg]
                                             #{value}))]
                       (if (-point<+ to
                                     to-seg)
                         (assoc tree-2
                                [from-seg to] (conj values
                                                    value)
                                [to to-seg]   values)
                         (if (= to
                                to-seg)
                           (-mark-join tree-2
                                       from-seg
                                       to-seg
                                       values
                                       value
                                       segments)
                           (recur (assoc tree-2
                                         segment
                                         (conj values
                                               value))
                                  to-seg
                                  to
                                  value
                                  segments))))))))




(defn mark 

  ""

  ;; A bit fugly and handcrafted, but does the job efficiently as it minimizes looping and hitting
  ;; the sorted-map, while preserving from unnecessary fragmentation.

  [tree from to value]

  (let [[[[from-seg
           to-seg
           :as segment]
          values] 
         & segments]    (subseq tree
                                >= from)]
    (if (or (nil? segment)
            (-disjoint? to
                        from-seg))
      (assoc tree
             [from to]
             #{value})
      (if (contains? values
                     value)
        (if (-point<- from
                      from-seg)
          (let [tree-2 (if (= (count values)
                              1)
                         (-> tree
                             (dissoc segment)
                             (assoc [from to-seg]
                                    values))
                         (assoc tree
                                [from from-seg]
                                #{value}))]
            (if (-point<=+ to
                           to-seg)
              tree-2
              (-mark-segments tree-2
                              to-seg
                              to
                              value
                              segments)))
          (if (-point<=+ to
                         to-seg)
            tree
            (-mark-segments tree
                            to-seg
                            to
                            value
                            segments)))
        (cond
          (= to
             from-seg)        (assoc tree
                                     [from to]
                                     #{value})
          (= from
             from-seg)        (if (-point<+ to
                                            to-seg)
                                (-> tree
                                    (dissoc segment)
                                    (assoc [from to] (conj values
                                                             value)
                                           [to to-seg] values))
                                (if (= to
                                       to-seg)
                                  (-mark-join tree
                                              from-seg
                                              to-seg
                                              values
                                              value
                                              segments)
                                  (-mark-segments (assoc tree
                                                         segment
                                                         (conj values
                                                               value))
                                                  to-seg
                                                  to
                                                  value
                                                  segments)))
          (-point<- from       
                    from-seg) (let [tree-2 (-> tree
                                               (dissoc segment)
                                               (assoc [from from-seg]
                                                      #{value}))]
                                (if (-point<+ to
                                              to-seg)
                                  (assoc tree-2
                                         [from-seg to] (conj values
                                                             value)
                                         [to to-seg]   values)
                                  (if (= to
                                         to-seg)
                                    (-mark-join tree-2
                                                from-seg
                                                to-seg
                                                values
                                                value
                                                segments)
                                    (-mark-segments (assoc tree-2
                                                           segment
                                                           (conj values
                                                                 value))
                                                    to-seg
                                                    to
                                                    value
                                                    segments))))
          :else               (let [tree-2 (-> tree
                                               (dissoc segment)
                                               (assoc [from-seg from]
                                                      values))]
                                (if (-point<+ to
                                              to-seg)
                                  (assoc tree-2
                                         [from to]  (conj values
                                                           value)
                                         [to to-seg] values)
                                  (if (= to
                                         to-seg)
                                    (-mark-join tree-2
                                                from
                                                to-seg
                                                values
                                                value
                                                segments)
                                    (-mark-segments (assoc tree-2
                                                           [from to-seg]
                                                           (conj values
                                                                 value))
                                                    to-seg
                                                    to
                                                    value
                                                    segments)))))))))



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
