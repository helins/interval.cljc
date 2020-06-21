(ns dvlopt.interval-test

  ""

  ;; Cf. https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core    :as clj]
            [clojure.test    :as t]
            [dvlopt.interval :as interval]))


;;;;;;;;;;


(t/deftest mark-init

  ;; Creating first segment in tree.

  (let [tree (interval/mark (interval/tree)
                            5
                            10
                            :x)]

    (t/is (= (sorted-map [5 10]
                         #{:x})
             tree)
          "Empty tree, 1 segment is created representing the first interval")

    (t/is (= #{:x}
             (get tree
                  5)
             (get tree
                  9))
          "X is retrieved within interval")

    (t/is (nil? (get tree
                     10))
          "Interval end is exclusive")))



(t/deftest mark-disjoint

  ;; Creating second segment in tree.

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :x)
                 (interval/mark 30
                                35
                                :y))]

    (t/is (= (sorted-map [5 10]  #{:x}
                         [30 35] #{:y})
             tree)
          "X < Y, 2 separate segments are created")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:x}
             (get tree
                  5)
             (get tree
                  8)))

    (t/is (nil? (get tree
                     10)))

    (t/is (= #{:y}
             (get tree
                  30)
             (get tree
                  34)))

    (t/is (nil? (get tree
                     35)))

    (t/is (nil? (get tree
                     40)))))



(t/deftest mark-meet

  ;; X MEETS Y tree

  (let [tree (-> (interval/tree)
                 (interval/mark 0
                                5
                                :x)
                 (interval/mark 5
                                10
                                :y))]

    (t/is (= (sorted-map [0 5]  #{:x}
                         [5 10] #{:y})
             tree)
          "X MEETS Y, 2 separate segments are created as the end of an interval is exclusive")

    (t/is (nil? (get tree
                     -1)))

    (t/is (= #{:x}
             (get tree
                  0)
             (get tree
                  4)))

    (t/is (= #{:y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))

    (t/is (nil? (get tree
                     15)))))



(t/deftest mark-equal

  ;; X = Y tree

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :x)
                 (interval/mark 5
                                10
                                :y))]

    (t/is (= (sorted-map [5 10]
                         #{:x
                           :y})
             tree)
          "X = Y, 1 segment is created and updated")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))

    (t/is (nil? (get tree
                     15)))))



(t/deftest mark-start

  ;; X STARTS Y and vice-versa, in various flavors

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 5
                                8
                                :x))]

    (t/is (= (sorted-map [5 8]  #{:x
                                  :y}
                         [8 10] #{:y})
             tree)
          "X STARTS Y, existing segment is split in 2, left subsegment updated")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  7)))

    (t/is (= #{:y}
             (get tree
                  8)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))

    (t/is (nil? (get tree
                     15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                nil
                                :y)
                 (interval/mark 5
                                10
                                :x))]

    (t/is (= (sorted-map [5 10]   #{:x
                                    :y}
                         [10 nil] #{:y})
             tree)
          "X STARTS Y which is half-open at the end, existing segment is split in 2, left subsegment updated")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (= #{:y}
             (get tree
                  10)
             (get tree
                  15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 5
                                15
                                :x))]

    (t/is (= (sorted-map [5 10]  #{:x
                                   :y}
                         [10 15] #{:x})
             tree)
          "Y STARTS X, existing segment updated, 1 created beyond for X")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))
    
    (t/is (= #{:x}
             (get tree
                  10)
             (get tree
                  14)))

    (t/is (nil? (get tree
                     15)))

    (t/is (nil? (get tree
                     20))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 5
                                nil
                                :x))]

    (t/is (= (sorted-map [5 10]   #{:y
                                    :x}
                         [10 nil] #{:x})
             tree)
        "Y STARTS X which is half-open at the end, existing segment updated, 1 created beyond for X")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (= #{:x}
             (get tree
                  10)
             (get tree
                  1000)))))



(t/deftest mark-finish


  ;; X FINISHES Y and vice-versa, in various flavors

  ;; Should be fine since marking is associative.

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 8
                                10
                                :x))]

    (t/is (= (sorted-map [5 8]  #{:y}
                         [8 10] #{:x
                                  :y})
             tree)
          "X FINISHES Y, existing segment split in 2, right subsegment updated")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:y}
             (get tree
                  5)
             (get tree
                  7)))

    (t/is (= #{:x
               :y}
             (get tree
                  8)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))
    
    (t/is (nil? (get tree
                     15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 2
                                10
                                :x))]

    (t/is (= (sorted-map [2 5]  #{:x}
                         [5 10] #{:x
                                  :y})
             tree)
          "Y FINISHES X, existing segment updated, 1 created before for X")

    (t/is (nil? (get tree
                     1)))

    (t/is (= #{:x}
             (get tree
                  2)
             (get tree
                  4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))

    (t/is (nil? (get tree
                     15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark nil
                                10
                                :x))]

    (t/is (= (sorted-map [nil 5] #{:x}
                         [5 10]  #{:y
                                   :x})
             tree)
          "Y FINISHES X which is half-open at start, existing segment updated, 1 created before for X")

    (t/is (= #{:x}
             (get tree
                  -10)
             (get tree
                  0)
             (get tree
                  4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))
    
    (t/is (nil? (get tree
                     15)))))



(t/deftest mark-during

  ;; X during Y and vice versa

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 2
                                12
                                :x))]

    (t/is (= (sorted-map [2 5]   #{:x}
                         [5 10]  #{:y
                                   :x}
                         [10 12] #{:x})
             tree)
          "Y DURING X, existing segment update, 1 created before for X, 1 beyond for X")

    (t/is (nil? (get tree
                     1)))

    (t/is (= #{:x}
             (get tree
                  2)
             (get tree
                  4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (= #{:x}
             (get tree
                  10)
             (get tree
                  11)))

    (t/is (nil? (get tree
                     12)))
    
    (t/is (nil? (get tree
                     15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 6
                                8
                                :x))]

    (t/is (= (sorted-map [5 6]  #{:y}
                         [6 8]  #{:y
                                  :x}
                         [8 10] #{:y})
             tree)
          "X DURING Y, existing segment split in 3, middle subsegment updated")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:y}
             (get tree
                  5)
             (get tree
                  5.5)))

    (t/is (= #{:x
               :y}
             (get tree
                  6)
             (get tree
                  7)))

    (t/is (= #{:y}
             (get tree
                  8)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))
    
    (t/is (nil? (get tree
                     15)))))



(t/deftest mark-overlap

  ;; X OVERLAPS Y and vice-versa

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 2
                                7
                                :x))]

    (t/is (= (sorted-map [2 5]  #{:x}
                         [5 7]  #{:x
                                  :y}
                         [7 10] #{:y})
             tree)
          "X OVERLAPS Y, existing segment split in 2, left subsegment udpated, 1 segment created before for X")

    (t/is (nil? (get tree
                     1)))

    (t/is (= #{:x}
             (get tree
                  2)
             (get tree
                  4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  6)))

    (t/is (= #{:y}
             (get tree
                  7)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))

    (t/is (nil? (get tree
                     15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark nil
                                7
                                :x))]

    (t/is (= (sorted-map [nil 5]  #{:x}
                         [5 7]    #{:y
                                    :x}
                         [7 10]   #{:y})
             tree)
          "X OVERLAPS y and is half-open at start, existing segment split in 2, left one updated, 1 segment created before for X")

    (t/is (= #{:x}
             (get tree
                  -1)
             (get tree
                  0)
             (get tree
                  4)))

    (t/is (= #{:x
               :y}
             (get tree
                  5)
             (get tree
                  6)))

    (t/is (= #{:y}
             (get tree
                  7)
             (get tree
                  9)))

    (t/is (nil? (get tree
                     10)))
    
    (t/is (nil? (get tree
                     15))))


  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :y)
                 (interval/mark 8
                                15
                                :x))]

    (t/is (= (sorted-map [5 8]   #{:y}
                         [8 10]  #{:y
                                   :x}
                         [10 15] #{:x})
             tree)
          "Y OVERLAPS X, existing segment split in 2, right subsegment updated, 1 segment created beyond for X")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:y}
             (get tree
                  5)
             (get tree
                  7)))

    (t/is (= #{:x
               :y}
             (get tree
                  8)
             (get tree
                  9)))

    (t/is (= #{:x}
             (get tree
                  10)
             (get tree
                  14)))

    (t/is (nil? (get tree
                     15)))

    (t/is (nil? (get tree
                     20)))))



(t/deftest mark-segments

  ;; Updating several segments during one mark

  (let [tree (-> (interval/tree)
                 (interval/mark 5
                                10
                                :a)
                 (interval/mark 10
                                15
                                :b)
                 (interval/mark 20
                                30
                                :c)
                 (interval/mark 5
                                35
                                :d))]

    (t/is (= (sorted-map [5 10]  #{:a
                                   :d}
                         [10 15] #{:b
                                   :d}
                         [20 30] #{:c
                                   :d}
                         [30 35] #{:d})
             tree)
          "X spans several segments, all are updated")

    (t/is (nil? (get tree
                     4)))

    (t/is (= #{:a
               :d}
             (get tree
                  5)
             (get tree
                  9)))

    (t/is (= #{:b
               :d}
             (get tree
                  10)
             (get tree
                  14)))

    (t/is (nil? (get tree
                     15)))

    (t/is (nil? (get tree
                     19)))


    (t/is (= #{:c
               :d}
             (get tree
                  20)
             (get tree
                  29)))

    (t/is (= #{:d}
             (get tree
                  30)
             (get tree
                  34)))

    (t/is (nil? (get tree
                     35)))
    
    (t/is (nil? (get tree
                     50)))

    (t/is (= [[[10 15] #{:b
                         :d}]
              [[20 30] #{:c
                         :d}]]
             (subseq tree
                     >= 10
                     <  30)
             (subseq tree
                     >= 12
                     <= 25)
             (subseq tree
                     > 9
                     < 30))
          "Querying segments in ways that should be equivalent given the state of the tree")

    (t/is (= [[[20 30] #{:c
                         :d}]
              [[10 15] #{:b
                         :d}]]
             (rsubseq tree
                      >= 10
                      <  30)
             (rsubseq tree
                      >= 12
                      <= 25))
          "Reverse segment querying")

    (t/is (= (seq tree)
             (subseq tree
                     >= nil)
             (subseq tree
                     > nil))
          "Querying segments starting at nil")

    (t/is (= (reverse (seq tree))
             (rsubseq tree
                      >= nil)
             (rsubseq tree
                      > nil))
          "Reverse segment querying starting at nil")

    (t/is (= [[[20 30] #{:c
                         :d}]
              [[30 35] #{:d}]]
             (subseq tree
                     >= 20)
             (subseq tree
                     >= 25)
             (subseq tree
                     > 19))
          "Querying segments after a given point")))



(t/deftest union

  (let [tree (-> (interval/tree)
                 (interval/mark 0
                                15
                                :a)
                 (interval/mark 12
                                nil
                                :b)
                 (interval/mark 20
                                25
                                :c)
                 (interval/mark 30
                                nil
                                :d)
                 (interval/mark 35
                                40
                                :e)
                 (interval/mark nil
                                0
                                :f))]

    (t/is (= #{:b
               :d
               :e}
             (interval/union (subseq tree
                                     >= 26))
             (interval/union (subseq tree
                                     >= 26
                                     <  45))
             (interval/union (subseq tree
                                     >= 26
                                     <= 35)))
          "Equivalent unions given current state of tree")

    (t/is (= #{:b
               :d
               :e}
             (interval/union (rsubseq tree
                                      >= 26))
             (interval/union (rsubseq tree
                                      >= 26
                                      <  45))
             (interval/union (rsubseq tree
                                      >= 26
                                      <= 35)))
          "Using reverse segment querying does not change anything")

    (t/is (= #{:b
               :d}
             (interval/union (subseq tree
                                     >= 1000000))
             (interval/union (rsubseq tree
                                      >= 1000000)))
          "Union of values at intervals with half-open ends")

    (t/is (= #{:f}
             (interval/union (subseq tree
                                     < 0))
             (interval/union (rsubseq tree
                                      < 0)))
          "Union of values at intervals with half-open starts")))
