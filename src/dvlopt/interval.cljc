(ns dvlopt.interval

  ""

  ;; Cf. http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
  ;;     https://github.com/Deep-Symmetry/beat-link-trigger/blob/master/src/beat_link_trigger/util.clj#L412

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


(defn- -segments

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



(defn- -split-at

  ""

  ([tree x]

   (if x
     (let [[[start
             end
             :as k] vs] (find tree
                              [x x])]
       (if (or (= start
                  x)
               (= end
                  x))
         tree
         (-> tree
             (clj/dissoc k)
             (clj/assoc [start x]
                        vs
                        [x end]
                        vs))))
     tree))


  ([tree from to]

   (-> tree
       (-split-at from)
       (-split-at to))))


;;;;;;;;;; Public API


(defn assoc

  ""

  [tree from to v]

  (let [tree-2 (-split-at tree
                          from
                          to)]
    (reduce (fn add [tree-3 [interval values]]
              (clj/assoc tree-3
                         interval
                         (conj values
                               v)))
            tree-2
            (-segments tree-2
                       from
                       to))))



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

  (sorted-map-by interval<
                 [nil nil]
                 #{}))
