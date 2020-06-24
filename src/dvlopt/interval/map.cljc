(ns dvlopt.interval.map

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
  ;;     https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core         :as clj]
            [clojure.set          :as clj.set]
            [dvlopt.interval.util :as interval.util]))


;;;;;;;;;; Marking intervals


(defn- -mark-merge-left

  ;; Former `-mark-join-pre`

  [tree from to values]

  (let [[[from-left
          to-left
          :as segment-left]
         values-left]       (first (rsubseq tree
                                             >= nil
                                             <  from))]
    (if (and values-left
             (= to-left
                from)
             (= values-left
                values))
      (-> tree
          (dissoc segment-left)
          (assoc [from-left to]
                 values))
      (assoc tree
             [from to]
             values))))


(defn- -mark-merge-right

  ;;

  [tree from to values segments]

  (let [[[from-right
          to-right
          :as segment-right]
         values-right]       (first segments)]
    (if (and values-right
             (= to
                from-right)
             (= values-right
                values))
      (-> tree
          (dissoc segment-right)
          (assoc [from to-right]
                 values))
      (assoc tree
             [from to]
             values))))



(defn- -mark-merge

  ;; Former `-mark-join`

  [tree from to values segments]

  (let [[[from-right
          to-right
          :as segment-right]
         values-right]       (first segments)]
    (if (and values-right
             (= to
                from-right)
             (= values-right
                values))
      (-mark-merge-left (dissoc tree
                                segment-right)
                        from
                        to-right
                        values)
      (-mark-merge-left tree
                        from
                        to
                        values))))



(defn- -markloop-merge-left

  ;;

  [tree from-acc to-acc values-acc assoc-acc? from to values]

  (if (and (= to-acc
              from)
           (= values-acc
              values))
    (assoc (if assoc-acc?
             tree
             (dissoc tree
                     [from-acc to-acc]))
           [from-acc to]
           values)
    (assoc (if assoc-acc?
             (assoc tree
                    [from-acc to-acc]
                    values-acc)
             tree)
           [from to]
           values)))



(defn- -markloop-merge-right

  ;;

  [tree from to values segments]

  (let [[[from-right
          to-right
          :as segment-right]
         values-right]       (first segments)]
    (if (and values-right
             (= to
                from-right)
             (= values-right
                values))
      (-> tree
          (dissoc segment-right)
          (assoc tree
                 [from to-right]
                 values))
      (assoc tree
             [from to]
             values))))



(defn- -markloop-merge

  ;;

  [tree from-acc to-acc values-acc assoc-acc? from to values segments]

  (if (and (= to-acc
              from)
           (= values-acc
              values))
    (-markloop-merge-right (if assoc-acc?
                             tree
                             (dissoc tree
                                     [from-acc to-acc]))
                           from-acc
                           to
                           values
                           segments)
    (-markloop-merge-right (if assoc-acc?
                             (assoc tree
                                    [from-acc to-acc]
                                    values-acc)
                             tree)
                           from
                           to
                           values
                           segments)))



(defn- -mark-rest

  ;;

  [tree from-2 to value from-acc to-acc values-acc assoc-acc? [[[from-seg
                                                                 to-seg
                                                                 :as segment]
                                                                values] 
                                                               & segments]]

  (if (or (nil? segment)
          (interval.util/disjoint? to
                                   from-seg))
    (-markloop-merge-left tree
                          from-acc
                          to-acc
                          values-acc
                          assoc-acc?
                          from-2
                          to
                          #{value})
    (if (contains? values
                   value)
      (if (interval.util/point<- from-2
                                 from-seg)
        (if (interval.util/point<=- to
                                    to-seg)
          (if (= (count values)
                 1)
            (-markloop-merge-left (dissoc tree
                                          segment)
                                  from-acc
                                  to-acc
                                  values-acc
                                  assoc-acc?
                                  from-2
                                  to-seg
                                  values)
            (-markloop-merge-left tree
                                  from-acc
                                  to-acc
                                  values-acc
                                  assoc-acc?
                                  from-2
                                  from-seg
                                  #{value}))
          (if (= (count values)
                 1)
            (if (and (= to-acc
                        from-2)
                     (= values-acc
                        values))
              (recur (let [tree-2 (dissoc tree
                                          segment)]
                       (if assoc-acc?
                         tree-2
                         (dissoc tree-2
                                 [from-acc to-acc])))
                     to-seg
                     to
                     value
                     from-acc
                     to-seg
                     values
                     true
                     segments)
              (recur (let [tree-2 (dissoc tree
                                          segment)]
                       (if assoc-acc?
                         (assoc tree-2
                                [from-acc to-acc]
                                values-acc)
                         tree-2))
                     to-seg
                     to
                     value
                     from-2
                     to-seg
                     values
                     true
                     segments))
            (recur (-markloop-merge-left tree
                                         from-acc
                                         to-acc
                                         values-acc
                                         assoc-acc?
                                         from-2
                                         from-seg
                                         #{value})
                   to-seg
                   to
                   value
                   from-seg
                   to-seg
                   values
                   false
                   segments)))
        (if (interval.util/point<=+ to
                                    to-seg)
          ;; TODO. Might not need to potentially merge left if `assoc-acc?` is true?
          (-markloop-merge-left tree
                                from-acc
                                to-acc
                                values-acc
                                assoc-acc?
                                from-seg
                                to-seg
                                values)
          (if (and (= to-acc
                      from-2)
                   (= values-acc
                      values))
            (recur (let [tree-2 (dissoc tree
                                        segment)]
                     (if assoc-acc?
                       tree-2
                       (dissoc tree-2
                               [from-acc to-acc])))
                   to-seg
                   to
                   value
                   from-acc
                   to-seg
                   values
                   true
                   segments)
            (recur (if assoc-acc?
                     (assoc tree
                            [from-acc to-acc])
                     tree)
                   to-seg
                   to
                   value
                   from-seg
                   to-seg
                   values
                   false
                   segments))))
      (cond
        (= to
           from-seg) (-markloop-merge-left tree
                                           from-acc
                                           to-acc
                                           values-acc
                                           assoc-acc?
                                           from-2
                                           to
                                           #{value})
        (= from-2
           from-seg) (if (interval.util/point<+ to
                                                to-seg)
                       (-> tree
                           (dissoc segment)
                           (assoc [to to-seg]
                                  values)
                           (-markloop-merge-left from-acc
                                                 to-acc
                                                 values-acc
                                                 assoc-acc?
                                                 from-2
                                                 to
                                                 (conj values
                                                       value)))
                       (if (= to
                              to-seg)
                         (-markloop-merge (dissoc tree
                                                  segment)
                                          from-acc
                                          to-acc
                                          values-acc
                                          assoc-acc?
                                          from-seg
                                          to-seg
                                          (conj values
                                                value)
                                          segments)
                         (let [values-2 (conj values
                                              value)]
                           (if (= values-acc
                                  values-2)
                             (recur (let [tree-2 (dissoc tree
                                                         segment)]
                                      (if assoc-acc?
                                        tree-2
                                        (dissoc tree
                                                [from-acc to-acc])))
                                    to-seg
                                    to
                                    value
                                    from-acc
                                    to-seg
                                    values-2
                                    true
                                    segments)
                             (recur (let [tree-2 (dissoc tree
                                                         segment)]
                                      (if assoc-acc?
                                        (assoc tree-2
                                               [from-acc to-acc] values-acc)
                                        tree-2))
                                    to-seg
                                    to
                                    value
                                    from-seg
                                    to-seg
                                    values-2
                                    true
                                    segments)))))
        :else         (let [tree-2 (-> tree
                                       (dissoc segment)
                                       (-markloop-merge-left from-acc
                                                             to-acc
                                                             values-acc
                                                             assoc-acc?
                                                             from-2
                                                             from-seg
                                                             #{value}))]
                       (if (interval.util/point<+ to
                                                  to-seg)
                         (assoc tree-2
                                [from-seg to] (conj values
                                                    value)
                                [to to-seg]   values)
                         (if (= to
                                to-seg)
                           (-mark-merge-right tree-2
                                              from-seg
                                              to-seg
                                              (conj values
                                                    value)
                                              segments)
                           (recur tree-2
                                  to-seg
                                  to
                                  value
                                  from-seg
                                  to-seg
                                  (conj values
                                        value)
                                  true
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
            (interval.util/disjoint? to
                                     from-seg))
      (-mark-merge-left tree
                        from
                        to
                        #{value})
      (if (contains? values
                     value)
        ;; Found segment contains target value
        (if (interval.util/point<- from
                                   from-seg)
          (if (interval.util/point<=+ to
                                      to-seg)
            (if (= (count values)
                   1)
              (-mark-merge-left (dissoc tree
                                        segment)
                                from
                                to-seg
                                values)
              (-mark-merge-left tree
                                from
                                from-seg
                                #{value}))
            (if (= (count values)
                   1)
              (let [[[from-left
                      to-left
                      :as segment-left]
                     values-left]       (first (rsubseq tree
                                                         >= nil
                                                         <  from))]
                (if (and values-left
                         (= to-left
                            from)
                         (= values-left
                            values))
                  (-mark-rest (dissoc tree
                                      segment-left
                                      segment)
                              to-seg
                              to
                              value
                              from-left
                              to-seg
                              values
                              true
                              segments)
                  (-mark-rest (dissoc tree
                                      segment)
                              to-seg
                              to
                              value
                              from
                              to-seg
                              values
                              true
                              segments)))
              (-mark-rest (-mark-merge-left tree
                                            from
                                            from-seg
                                            #{value})
                          to-seg
                          to
                          value
                          from-seg
                          to-seg
                          values
                          false
                          segments)))
          (if (interval.util/point<=+ to
                                      to-seg)
            tree
            (-mark-rest tree
                        to-seg
                        to
                        value
                        from-seg
                        to-seg
                        values
                        false
                        segments)))
        ;; Found segment does not contain target value
        (cond
          (= to
             from-seg)        (-mark-merge-left tree
                                                from
                                                to
                                                #{value})
          (= from
             from-seg)        (if (interval.util/point<+ to
                                                         to-seg)
                                (-mark-merge-left (-> tree
                                                      (dissoc segment)
                                                      (assoc [to to-seg]
                                                             values))
                                                  from
                                                  to
                                                  (conj values
                                                        value))
                                (if (= to
                                       to-seg)
                                  (-mark-merge (dissoc tree
                                                       segment)
                                               from-seg
                                               to-seg
                                               (conj values
                                                     value)
                                               segments)
                                  (let [[[from-left
                                          to-left
                                          :as segment-left]
                                         values-left]       (first (rsubseq tree
                                                                             >= nil
                                                                             <  from))]
                                    (let [values-2 (conj values
                                                         value)]
                                      (if (and values-left
                                               (= to-left
                                                  from)
                                               (= values-left
                                                  values-2))
                                        (-mark-rest (dissoc tree
                                                            segment-left
                                                            segment)
                                                    to-seg
                                                    to
                                                    value
                                                    from-left
                                                    to-seg
                                                    values-2
                                                    true
                                                    segments)
                                        (-mark-rest (dissoc tree
                                                            segment)
                                                    to-seg
                                                    to
                                                    value
                                                    from-seg
                                                    to-seg
                                                    (conj values
                                                          value)
                                                    true
                                                    segments))))))
          (interval.util/point<-
            from       
            from-seg)         (let [tree-2 (-mark-merge-left (dissoc tree
                                                                     segment)
                                                             from
                                                             from-seg
                                                             #{value})]
                                (if (interval.util/point<+ to
                                                           to-seg)
                                  (assoc tree-2
                                         [from-seg to] (conj values
                                                             value)
                                         [to to-seg]   values)
                                  (if (= to
                                         to-seg)
                                    (-mark-merge-right tree-2
                                                       from-seg
                                                       to-seg
                                                       (conj values
                                                             value)
                                                       segments)
                                    (-mark-rest tree-2
                                                to-seg
                                                to
                                                value
                                                from-seg
                                                to-seg
                                                (conj values
                                                      value)
                                                true
                                                segments))))
          :else               (let [tree-2 (-> tree
                                               (dissoc segment)
                                               (assoc [from-seg from]
                                                      values))]
                                (if (interval.util/point<+ to
                                                           to-seg)
                                  (assoc tree-2
                                         [from to]   (conj values
                                                           value)
                                         [to to-seg] values)
                                  (if (= to
                                         to-seg)
                                    (-mark-merge-right tree-2
                                                       from
                                                       to
                                                       (conj values
                                                             value)
                                                       segments)
                                    (-mark-rest tree-2
                                                to-seg
                                                to
                                                value
                                                from
                                                to-seg
                                                (conj values
                                                      value)
                                                true
                                                segments)))))))))


;;;;;;;;;; Erasing intervals


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
           (interval.util/overlapping? to
                                       from-seg))
    (if (contains? values
                   value)
      (if (interval.util/point<+ to
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
      (if (interval.util/point<=+ to
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
             (interval.util/overlapping? to
                                         from-seg))
      (if (contains? values
                     value)
        (if (interval.util/point<=- from
                                    from-seg)
          (if (interval.util/point<+ to
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
            (if (interval.util/point<+ to
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
        (if (interval.util/point<=+ to
                                    to-seg)
          tree
          (-erase-segments value
                           to
                           tree
                           segments)))
      tree)))


;;;;;;;;;; Rest of public API


(defn tree

  ""

  []

  (sorted-map-by interval.util/cmp))



(defn union

  ""

  [segments]

  (reduce (fn dedup-values [values [_segment values-segment]]
            (clj.set/union values
                           values-segment))
          #{}
          segments))
