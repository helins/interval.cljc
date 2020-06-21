(ns dvlopt.interval

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
  ;;     https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core :as clj]
            [clojure.set  :as clj.set])
  ;;
  ;; <!> Attention, highly confusing when not kept in mind <!>
  ;;
  (:refer-clojure :exclude [assoc
                            dissoc
                            get]))


;;;;;;;;;; Private

  ;; TODO. Make private

(defn -segments


  ""

  [tree from to]

  (cond 
    (and from
         to)  (subseq tree
                      >=
                      [from from]
                      <
                      [to to])
    from      (subseq tree 
                      >=
                      [from from])
    to        (subseq tree
                      <
                      [to to])
    :else     tree))



(defn -split-at

  ""

  ([tree x]

   (if x
     (if-some [[[start
                 end
                 :as interval]
                vs]            (find tree
                                     [x x])]
       (if (or (= start
                  x)
               (= end
                  x))
         tree
         (-> tree
             (clj/dissoc interval)
             (clj/assoc [start x]
                        vs
                        [x end]
                        vs)))
       tree)
     tree))


  ([tree from to]

   (-> tree
       (-split-at from)
       (-split-at to))))


;;;;;;;;;; Public API


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



(defn- -assoc-segments

  ;;

  [value from-2 to tree [[[from-seg
                           to-seg
                           :as segment]
                          values]
                         & segments]]
  (if segment
    (cond
      (= to
         to-seg)        (clj/assoc tree
                                   segment
                                   (conj values
                                         value))
      (-point<+ to
                to-seg) (-> tree
                            (clj/dissoc segment)
                            (clj/assoc [from-seg to] (conj values
                                                           value)
                                       [to to-seg]   values))
      :else             (recur value
                               to-seg
                               to
                               (clj/assoc tree
                                          segment
                                          (conj values
                                                value))
                               segments))
    (clj/assoc tree
               [from-2 to]
               #{value})))



(defn assoc

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

  [tree from to value]

  (let [[[[from-seg
           to-seg
           :as segment]
          values] 
         & segments]    (subseq tree
                                >=
                                [from from])]
    (if (or (nil? segment)
            (and (some? from-seg)
                 (some? to)
                 (<= to
                     from-seg)))
      (clj/assoc tree
                 [from to]
                 #{value})
      (cond
        (= from
           from-seg)        (cond
                              (= to
                                 to-seg)        (clj/assoc tree
                                                           segment
                                                           (conj values
                                                                 value))
                              (-point<+ to
                                        to-seg) (-> tree
                                                    (clj/dissoc segment)
                                                    (clj/assoc [from to]   (conj values
                                                                                 value)
                                                               [to to-seg] values))
                              :else
                              (-assoc-segments value
                                               to-seg
                                               to
                                               (clj/assoc tree
                                                          segment
                                                          (conj values
                                                                value))
                                               segments))
        (-point<- from       
                  from-seg) (let [tree-2 (-> tree
                                             (clj/dissoc segment)
                                             (clj/assoc [from from-seg]
                                                        #{value}))]
                              (cond
                                (= to
                                   to-seg)        (clj/assoc tree-2
                                                             segment
                                                             (conj values
                                                                   value))
                                (-point<+ to
                                          to-seg) (clj/assoc tree-2
                                                             [from-seg to] (conj values
                                                                                 value)
                                                             [to to-seg]   values)
                                :else
                                (-assoc-segments value
                                                 to-seg
                                                 to
                                                 (clj/assoc tree-2
                                                            segment
                                                            (conj values
                                                                  value))
                                                 segments)))
        :else              (let [tree-2 (-> tree
                                            (clj/dissoc segment)
                                            (clj/assoc [from-seg from]
                                                       values))]
                             (cond
                               (= to
                                  to-seg)        (clj/assoc tree-2
                                                            [from to]
                                                            (conj values
                                                                  value))
                               (-point<+ to
                                         to-seg) (clj/assoc tree-2
                                                            [from to]   (conj values
                                                                              value)
                                                            [to to-seg] values)
                               :else
                               (-assoc-segments value
                                                to-seg
                                                to
                                                (clj/assoc tree-2
                                                           [from to-seg]
                                                           (conj values
                                                                 value))
                                                segments)))))))



(defn dissoc

  ""

  [tree from to v]

  (let [tree-2 (-split-at tree
                          from
                          to)]
    (reduce (fn remove [tree-3 [interval values]]
              (let [values-2 (disj values
                                   v)]
                (if (empty? values-2)
                  (clj/dissoc tree-3
                              interval)
                  (clj/assoc tree-3
                             interval
                             values-2))))
            tree-2
            (-segments tree-2
                       from
                       to))))



(defn get

  ""

  ([tree x]

   (clj/get tree
            [x x]))


  ([tree from to]

   (reduce (fn dedup-values [result [_ vs]]
             (clj.set/union result
                            vs))
           #{}
           (-segments tree
                      from
                      to))))


;;;;;;;;;; Creating an interval tree


(defn interval<

  ""

  [[start-a end-a] [start-b _]]

  (boolean (and end-a
                start-b
                (if (= start-a
                       end-a)
                  (< end-a
                     start-b)
                  (<= end-a
                      start-b)))))



(defn tree

  ""

  []

  (sorted-map-by interval<))
                 ;;[nil nil]
                 ;;#{}))
