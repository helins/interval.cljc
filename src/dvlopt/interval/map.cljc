(ns dvlopt.interval.map

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-imap/
  ;;     https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core         :as clj]
            [clojure.set          :as clj.set]
            [dvlopt.interval.util :as interval.util])
  (:refer-clojure :exclude [empty]))


;; TODO. Avoid unnecessary dissoc and re-assoc when using the merging helpers.


;;;;;;;;;; Marking intervals


(defn- -merge-left?

  ;;

  [imap from values]

  (when values
    (let [[[from-left
            to-left
            :as segment-left]
           values-left]       (first (rsubseq imap
                                              >= nil
                                              <  from))]
      (when (and values-left
                 (= to-left
                    from)
                 (= values-left
                    values))
        [from-left to-left]))))




(defn- -mark-merge-left

  ;;

  [imap from to values]

  (let [[[from-left
          to-left
          :as segment-left]
         values-left]       (first (rsubseq imap
                                             >= nil
                                             <  from))]
    (if (and values-left
             (= to-left
                from)
             (= values-left
                values))
      (-> imap
          (dissoc segment-left)
          (assoc [from-left to]
                 values))
      (assoc imap
             [from to]
             values))))


(defn- -mark-merge-right

  ;;

  [imap from to values segments]

  (let [[[from-right
          to-right
          :as segment-right]
         values-right]       (first segments)]
    (if (and values-right
             (= to
                from-right)
             (= values-right
                values))
      (-> imap
          (dissoc segment-right)
          (assoc [from to-right]
                 values))
      (assoc imap
             [from to]
             values))))



(defn- -mark-merge

  ;;

  [imap from to values segments]

  (let [[[from-right
          to-right
          :as segment-right]
         values-right]       (first segments)]
    (if (and values-right
             (= to
                from-right)
             (= values-right
                values))
      (-mark-merge-left (dissoc imap
                                segment-right)
                        from
                        to-right
                        values)
      (-mark-merge-left imap
                        from
                        to
                        values))))



(defn- -markloop-merge-left

  ;;

  [imap from-acc to-acc values-acc assoc-acc? from to values]

  (if (and (= to-acc
              from)
           (= values-acc
              values))
    (assoc (if assoc-acc?
             imap
             (dissoc imap
                     [from-acc to-acc]))
           [from-acc to]
           values)
    (assoc (if assoc-acc?
             (assoc imap
                    [from-acc to-acc]
                    values-acc)
             imap)
           [from to]
           values)))



(defn- -markloop-merge-right

  ;; TODO. Refactor, now identical to [[-mark-merge-right]]

  [imap from to values segments]

  (let [[[from-right
          to-right
          :as segment-right]
         values-right]       (first segments)]
    (if (and values-right
             (= to
                from-right)
             (= values-right
                values))
      (-> imap
          (dissoc segment-right)
          (assoc [from to-right]
                 values))
      (assoc imap
             [from to]
             values))))



(defn- -markloop-merge

  ;;

  [imap from-acc to-acc values-acc assoc-acc? from to values segments]

  (if (and (= to-acc
              from)
           (= values-acc
              values))
    (-markloop-merge-right (if assoc-acc?
                             imap
                             (dissoc imap
                                     [from-acc to-acc]))
                           from-acc
                           to
                           values
                           segments)
    (-markloop-merge-right (if assoc-acc?
                             (assoc imap
                                    [from-acc to-acc]
                                    values-acc)
                             imap)
                           from
                           to
                           values
                           segments)))



(defn- -mark-rest

  ;;

  [imap from-2 to value from-acc to-acc values-acc assoc-acc? [[[from-seg
                                                                 to-seg
                                                                 :as segment]
                                                                values] 
                                                               & segments]]

  (if (or (nil? segment)
          (interval.util/disjoint? to
                                   from-seg))
    (-markloop-merge-left imap
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
            (-markloop-merge-left (dissoc imap
                                          segment)
                                  from-acc
                                  to-acc
                                  values-acc
                                  assoc-acc?
                                  from-2
                                  to-seg
                                  values)
            (-markloop-merge-left imap
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
              (recur (let [imap-2 (dissoc imap
                                          segment)]
                       (if assoc-acc?
                         imap-2
                         (dissoc imap-2
                                 [from-acc to-acc])))
                     to-seg
                     to
                     value
                     from-acc
                     to-seg
                     values
                     true
                     segments)
              (recur (let [imap-2 (dissoc imap
                                          segment)]
                       (if assoc-acc?
                         (assoc imap-2
                                [from-acc to-acc]
                                values-acc)
                         imap-2))
                     to-seg
                     to
                     value
                     from-2
                     to-seg
                     values
                     true
                     segments))
            (recur (-markloop-merge-left imap
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
          (-markloop-merge-left (dissoc imap
                                        segment)
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
            (recur (let [imap-2 (dissoc imap
                                        segment)]
                     (if assoc-acc?
                       imap-2
                       (dissoc imap-2
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
                     (assoc imap
                            [from-acc to-acc]
                            values-acc)
                     imap)
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
           from-seg) (-markloop-merge-left imap
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
                       (-> imap
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
                         (-markloop-merge (dissoc imap
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
                             (recur (let [imap-2 (dissoc imap
                                                         segment)]
                                      (if assoc-acc?
                                        imap-2
                                        (dissoc imap-2
                                                [from-acc to-acc])))
                                    to-seg
                                    to
                                    value
                                    from-acc
                                    to-seg
                                    values-2
                                    true
                                    segments)
                             (recur (let [imap-2 (dissoc imap
                                                         segment)]
                                      (if assoc-acc?
                                        (assoc imap-2
                                               [from-acc to-acc] values-acc)
                                        imap-2))
                                    to-seg
                                    to
                                    value
                                    from-seg
                                    to-seg
                                    values-2
                                    true
                                    segments)))))
        :else         (let [imap-2 (-> imap
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
                         (assoc imap-2
                                [from-seg to] (conj values
                                                    value)
                                [to to-seg]   values)
                         (if (= to
                                to-seg)
                           (-mark-merge-right imap-2
                                              from-seg
                                              to-seg
                                              (conj values
                                                    value)
                                              segments)
                           (recur imap-2
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

  [imap from to value]

  (let [[[[from-seg
           to-seg
           :as segment]
          values] 
         & segments]    (subseq imap
                                >= from)]
    (if (or (nil? segment)
            (interval.util/disjoint? to
                                     from-seg))
      (-mark-merge-left imap
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
              (-mark-merge-left (dissoc imap
                                        segment)
                                from
                                to-seg
                                values)
              (-mark-merge-left imap
                                from
                                from-seg
                                #{value}))
            (if (= (count values)
                   1)
              (let [[[from-left
                      to-left
                      :as segment-left]
                     values-left]       (first (rsubseq imap
                                                         >= nil
                                                         <  from))]
                (if (and values-left
                         (= to-left
                            from)
                         (= values-left
                            values))
                  (-mark-rest (dissoc imap
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
                  (-mark-rest (dissoc imap
                                      segment)
                              to-seg
                              to
                              value
                              from
                              to-seg
                              values
                              true
                              segments)))
              (-mark-rest (-mark-merge-left imap
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
            imap
            (-mark-rest imap
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
             from-seg)        (-mark-merge-left imap
                                                from
                                                to
                                                #{value})
          (= from
             from-seg)        (if (interval.util/point<+ to
                                                         to-seg)
                                (-mark-merge-left (-> imap
                                                      (dissoc segment)
                                                      (assoc [to to-seg]
                                                             values))
                                                  from
                                                  to
                                                  (conj values
                                                        value))
                                (if (= to
                                       to-seg)
                                  (-mark-merge (dissoc imap
                                                       segment)
                                               from-seg
                                               to-seg
                                               (conj values
                                                     value)
                                               segments)
                                  (let [[[from-left
                                          to-left
                                          :as segment-left]
                                         values-left]       (first (rsubseq imap
                                                                             >= nil
                                                                             <  from))]
                                    (let [values-2 (conj values
                                                         value)]
                                      (if (and values-left
                                               (= to-left
                                                  from)
                                               (= values-left
                                                  values-2))
                                        (-mark-rest (dissoc imap
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
                                        (-mark-rest (dissoc imap
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
            from-seg)         (let [imap-2 (-mark-merge-left (dissoc imap
                                                                     segment)
                                                             from
                                                             from-seg
                                                             #{value})]
                                (if (interval.util/point<+ to
                                                           to-seg)
                                  (assoc imap-2
                                         [from-seg to] (conj values
                                                             value)
                                         [to to-seg]   values)
                                  (if (= to
                                         to-seg)
                                    (-mark-merge-right imap-2
                                                       from-seg
                                                       to-seg
                                                       (conj values
                                                             value)
                                                       segments)
                                    (-mark-rest imap-2
                                                to-seg
                                                to
                                                value
                                                from-seg
                                                to-seg
                                                (conj values
                                                      value)
                                                true
                                                segments))))
          :else               (let [imap-2 (-> imap
                                               (dissoc segment)
                                               (assoc [from-seg from]
                                                      values))]
                                (if (interval.util/point<+ to
                                                           to-seg)
                                  (assoc imap-2
                                         [from to]   (conj values
                                                           value)
                                         [to to-seg] values)
                                  (if (= to
                                         to-seg)
                                    (-mark-merge-right imap-2
                                                       from
                                                       to
                                                       (conj values
                                                             value)
                                                       segments)
                                    (-mark-rest imap-2
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


(defn- -erase-merge-left

  ;;

  [imap from to values]

  (if (empty? values)
    imap
    (-mark-merge-left imap
                      from
                      to
                      values)))

 

(defn- -erase-merge-right

  ;;

  [imap from to values segments]

  (if (empty? values)
    imap
    (-mark-merge-right imap
                       from
                       to
                       values
                       segments)))



(defn- -erase-merge

  ;;

  [imap from to values segments]

  (if (empty? values)
    imap
    (-mark-merge imap
                 from
                 to
                 values
                 segments)))

 



(defn- -eraseloop-acc

  ;;

  [imap from-acc to-acc values-acc assoc-acc?]

  (if (and values-acc
           assoc-acc?)
    (assoc imap
           [from-acc to-acc]
           values-acc)
    imap))




(defn- -eraseloop-merge-left

  ;;

  [imap from-acc to-acc values-acc assoc-acc? from to values]

  (if (empty? values)
    (if (and values-acc
             assoc-acc?)
      (assoc imap
             [from-acc to-acc]
             values-acc)
      imap)
    (if values-acc
      (-markloop-merge-left imap
                            from-acc
                            to-acc
                            values-acc
                            assoc-acc?
                            from
                            to
                            values)
      imap)))



(defn- -eraseloop-merge

  ;;

  [imap from-acc to-acc values-acc assoc-acc? from to values segments]

  (if (empty? values)
    (if assoc-acc?
      (assoc imap
             [from-acc to-acc]
             values-acc)
      imap)
    (if values-acc
      (-markloop-merge imap
                       from-acc
                       to-acc
                       values-acc
                       assoc-acc? from
                       to
                       values
                       segments)
      imap)))








(defn- -erase-value

  ;;

  [imap segment values value]

  (let [values-2 (disj values
                       value)]
    (if (empty? values-2)
      (dissoc imap
              segment)
      (assoc imap
             segment
             values-2))))



(defn- -restore-values

  ;;

  [imap segment values value]

  (let [values-2 (disj values
                       value)]
    (if (empty? values-2)
      imap
      (assoc imap
             segment
             values-2))))



(defn- -erase-rest

  ;; Used by [[erase]].
  ;;
  ;; Behaves very much like [[erase]] but optimizes the algorithm as it is not dealing with the first
  ;; segment anymore. Is pretty much a copy/paste, but the recursion and all the destructuring
  ;; make it not worth the trouble abstracting that away.

  [imap from-2 to value from-acc to-acc values-acc assoc-acc? [[[from-seg
                                                                 to-seg
                                                                 :as segment]
                                                                values]
                                                               & segments]]

  (if (and segment
           (interval.util/overlapping? to
                                       from-seg))
    (if (contains? values
                   value)
      (if (interval.util/point<- from-2
                                 from-seg)
        (if (interval.util/point<+ to
                                   to-seg)
          (-> imap
              (dissoc segment)
              (assoc [to to-seg]
                     values)
              (-eraseloop-acc from-acc
                              to-acc
                              values-acc
                              assoc-acc?)
              (-restore-values [from-seg to]
                               values
                               value))
          (if (= to
                 to-seg)
            (-> imap
                (-eraseloop-acc from-acc
                                to-acc
                                values-acc
                                assoc-acc?)
                (-erase-merge-right from-seg
                                    to-seg
                                    (disj values
                                          value)
                                    segments))
            (recur (-> imap
                       (dissoc segment)
                       (-eraseloop-acc from-acc
                                       to-acc
                                       values-acc
                                       assoc-acc?))
                   to-seg
                   to
                   value
                   from-seg
                   to-seg
                   (not-empty (disj values
                                    value))
                   true
                   segments)))
        ;; (= from-seg to-seg)
        (if (interval.util/point<+ to
                                   to-seg)
          (-eraseloop-merge-left (-> imap
                                     (dissoc segment)
                                     (assoc [to to-seg]
                                            values))
                                 from-acc
                                 to-acc
                                 values-acc
                                 assoc-acc?
                                 from-seg
                                 to
                                 (disj values
                                       value))
          (if (= to
                 to-seg)
            (-eraseloop-merge (dissoc imap
                                      segment)
                              from-acc
                              to-acc
                              values-acc
                              assoc-acc?
                              from-2
                              to-seg
                              (disj values
                                    value)
                              segments)
            (let [values-2 (not-empty (disj values
                                            value))]
              (if values-2
                (if (and values-acc
                         (= to-acc
                            from-2)
                         (= values-acc
                            values-2))
                  (recur (let [imap-2 (dissoc imap
                                              segment)]
                           (if assoc-acc?
                             imap-2
                             (dissoc imap-2
                                     [from-acc to-acc])))
                         to-seg
                         to
                         value
                         from-acc
                         to-seg
                         values-2
                         true
                         segments)
                  (recur (-> imap
                             (dissoc segment)
                             (-eraseloop-acc from-acc
                                             to-acc
                                             values-acc
                                             assoc-acc?))
                         to-seg
                         to
                         value
                         from-seg
                         to-seg
                         values-2
                         true
                         segments))
                (recur (-> imap
                           (dissoc segment)
                           (-eraseloop-acc from-acc
                                           to-acc
                                           values-acc
                                           assoc-acc?))
                       to-seg
                       to
                       value
                       from-seg
                       to-seg
                       values-2
                       true
                       segments))))))
      (if (interval.util/point<+ from-2
                                 from-seg)
        (let [imap-2 (-eraseloop-acc imap
                                     from-acc
                                     to-acc
                                     values-acc
                                     assoc-acc?)]
          (if (interval.util/point<=+ to
                                      to-seg)
            imap-2
            (recur imap-2
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
          (if (empty? values)
              (if (and values-acc
                       assoc-acc?)
                (assoc imap
                       [from-acc to-acc]
                       values-acc)
                imap)
              (if values-acc
                (-markloop-merge-left (dissoc imap
                                              segment)
                                      from-acc
                                      to-acc
                                      values-acc
                                      assoc-acc?
                                      from-seg
                                      to-seg
                                      values)
                imap))

          ; (-eraseloop-merge-left (dissoc imap
          ;                                segment)
          ;                        from-acc
          ;                        to-acc
          ;                        values-acc
          ;                        assoc-acc?
          ;                        from-seg
          ;                        to-seg
          ;                        values)
          (if (and values-acc
                   (= to-acc
                      from-2)
                   (= values-acc
                      values))
            (recur (let [imap-2 (dissoc imap
                                        segment)]
                     (if assoc-acc?
                       imap-2
                       (dissoc imap-2
                               [from-acc to-acc])))
                   to-seg
                   to
                   value
                   from-acc
                   to-seg
                   values
                   true
                   segments)
            (recur (-eraseloop-acc imap
                                   from-acc
                                   to-acc
                                   values-acc
                                   assoc-acc?)
                   to-seg
                   to
                   value
                   from-seg
                   to-seg
                   values
                   false
                   segments)))))
    (-eraseloop-acc imap
                    from-acc
                    to-acc
                    values-acc
                    assoc-acc?)))



(defn erase

  ""

  [imap from to value]

  (let [[[[from-seg
           to-seg
           :as segment]
          values]
         & segments]    (subseq imap
                                >= from)]
    (if (and segment
             (interval.util/overlapping? to
                                         from-seg))
      (if (contains? values
                     value)
        (if (interval.util/point<- from
                                   from-seg)
          (if (interval.util/point<+ to
                                     to-seg)
            (-restore-values (-> imap
                                 (dissoc segment)
                                 (assoc [to to-seg]
                                        values))
                             [from-seg to]
                             values
                             value)
            (if (= to
                   to-seg)
              (-erase-merge-right (dissoc imap
                                          segment)
                                  from-seg
                                  to-seg
                                  (disj values
                                        value)
                                  segments)
              (-erase-rest (dissoc imap
                                   segment)
                           to-seg
                           to
                           value
                           from-seg
                           to-seg
                           (not-empty (disj values
                                            value))
                           true
                           segments)))
          (if (= from
                 from-seg)
            (if (interval.util/point<+ to
                                       to-seg)
              (-erase-merge-left (-> imap
                                     (dissoc segment)
                                     (assoc [to to-seg]
                                            values))
                                 from
                                 to
                                 (disj values
                                       value))
              (if (= to
                     to-seg)
                (-erase-merge (dissoc imap
                                      segment)
                              from
                              to
                              (disj values
                                    value)
                              segments)
                (let [values-2 (not-empty (disj values
                                                value))]
                  (if-some [[from-left
                             _to-left
                             :as segment-left] (-merge-left? imap
                                                             from
                                                             values-2)]
                    (-erase-rest (dissoc imap
                                         segment
                                         segment-left)
                                 to-seg
                                 to
                                 value
                                 from-left
                                 to-seg
                                 values-2
                                 true
                                 segments)
                    (-erase-rest (dissoc imap
                                         segment)
                                 to-seg
                                 to
                                 value
                                 from-seg
                                 to-seg
                                 values-2
                                 true
                                 segments)))))
            (if (interval.util/point<+ to
                                       to-seg)
              (-restore-values (-> imap
                                   (dissoc segment)
                                   (assoc [from-seg from]
                                          values)
                                   (assoc [to to-seg]
                                          values))
                               [from to]
                               values
                               value)
              (if (= to
                     to-seg)
                (-erase-merge-right (-> imap
                                        (dissoc segment)
                                        (assoc [from-seg from]
                                               values))
                                    from
                                    to-seg
                                    (disj values
                                          value)
                                    segments)
                (-erase-rest (-> imap
                                 (dissoc segment)
                                 (assoc [from-seg from]
                                        values))
                             to-seg
                             to
                             value
                             from
                             to-seg
                             (not-empty (disj values
                                              value))
                             true
                             segments)))))
        (if (interval.util/point<=+ to
                                    to-seg)
          imap
          (-erase-rest imap
                       to-seg
                       to
                       value
                       from-seg
                       to-seg
                       values
                       false
                       segments)))
      imap)))


;;;;;;;;;; Rest of public API


(def empty

  ""

  (sorted-map-by interval.util/cmp))



(defn union

  ""

  [segments]

  (reduce (fn dedup-values [values [_segment values-segment]]
            (clj.set/union values
                           values-segment))
          #{}
          segments))
