(ns dvlopt.interval.map-test

  ""

  ;; Cf. https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core        :as clj]
            [clojure.test        :as t]
            [dvlopt.interval.map :as interval.map]))


;; TODO. Open intervals

;;;;;;;;;;


(t/deftest mark-init

  ;; Creating first segment in imap.

  (let [imap (interval.map/mark interval.map/empty
                                5
                                10
                                :x)]

    (t/is (= (seq (sorted-map [5 10]
                              #{:x}))
             (seq imap))
          "Empty imap, 1 segment is created representing the first interval")

    (t/is (= #{:x}
             (get imap
                  5)
             (get imap
                  9))
          "X is retrieved within interval")

    (t/is (nil? (get imap
                     10))
          "Interval end is exclusive")

    (t/is (empty? (interval.map/erase imap
                                      5
                                      10
                                      :x))
          "Erasing a whole segment")

    (t/is (= (seq imap)
             (seq (interval.map/erase imap
                                      5
                                      10
                                      :y)))
          "Segment is left intact if it does not contain the target value")

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     5
                                     7
                                     :x))
             (seq (interval.map/erase imap
                                      7
                                      10
                                      :x)))
          "FINISH erasing")

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     7
                                     10
                                     :x))
             (seq (interval.map/erase imap
                                      5
                                      7
                                      :x)))
          "START erasing")

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         6
                                         :x)
                      (interval.map/mark 9
                                         10
                                         :x)))
             (seq (interval.map/erase imap
                                      6
                                      9
                                      :x)))
          "DURING erasing")

    (t/is (= (seq imap)
             (seq (interval.map/erase imap
                                      0
                                      3
                                      :x)))
          "< erasing")

    (t/is (= (seq imap)
             (seq (interval.map/erase imap
                                      10
                                      15
                                      :x)))
          "> erasing")

    (t/is (= (seq imap)
             (seq (interval.map/erase imap
                                      0
                                      5
                                      :x)))
          "MEET erasing")

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     7
                                     10
                                     :x))
             (seq (interval.map/erase imap
                                      0
                                      7
                                      :x)))
          "OVERLAP erasing")

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     7
                                     10
                                     :x))
             (seq (interval.map/erase imap
                                      nil
                                      7
                                      :x)))
          "OVERLAP erasing (half-open)")

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     5
                                     7
                                     :x))
             (seq (interval.map/erase imap
                                      7
                                      15
                                      :x)))
          "INV OVERLAP erasing")

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     5
                                     7
                                     :x))
             (seq (interval.map/erase imap
                                      7
                                      nil
                                      :x)))
          "INV OVERLAP erasing (half-open)")))



(t/deftest mark-disjoint

  ;; Creating second segment in imap.

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :x)
                 (interval.map/mark 30
                                    35
                                    :y))]

    (t/is (= (seq (sorted-map [5 10]  #{:x}
                              [30 35] #{:y}))
             (seq imap))
          "X < Y, 2 separate segments are created")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:x}
             (get imap
                  5)
             (get imap
                  8)))

    (t/is (nil? (get imap
                     10)))

    (t/is (= #{:y}
             (get imap
                  30)
             (get imap
                  34)))

    (t/is (nil? (get imap
                     35)))

    (t/is (nil? (get imap
                     40)))

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         8
                                         :x)
                      (interval.map/mark 30
                                         35
                                         :y)))
             (seq (interval.map/erase imap
                                      8
                                      33
                                      :x)))
          "Erasing X across disjoint segments, non-containing segments left intact")))



(t/deftest mark-disjoint-defrag

   (t/is (= (seq (-> interval.map/empty
                     (interval.map/mark 0
                                        10
                                        :x)
                     (interval.map/mark 20
                                        30
                                        :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 20
                                       30
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :x))))
        "INV MEETS, disjoint with right segment"))



(t/deftest mark-rest-disjoint

  ;; Also tests several ways of going from [[mark]] to [[-mark-rest]].

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       13
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 0
                                       13
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 3
                                       13
                                       :x))))
        "Left merge, initial segments contains target values")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/mark 10
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 5
                                       15
                                       :y))))
        "STARTS with first segment")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       8
                                       :x)
                    (interval.map/mark 8
                                       10
                                       :y)
                    (interval.map/mark 8
                                       10
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 8
                                       15
                                       :y))))
        "FINISHES first segment")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :y)
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/mark 10
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 0
                                       15
                                       :y))))
        "OVERLAPS first segment")
                                   

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   0
                                   15
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 0
                                       15
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       3
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 3
                                       15
                                       :x))))
        "OVERLAPS initial segment + idem with left merge"))



(t/deftest mark-meet

  ;; X MEETS Y imap

  (let [imap (-> interval.map/empty
                 (interval.map/mark 0
                                    5
                                    :x)
                 (interval.map/mark 5
                                    10
                                    :y))]

    (t/is (= (seq (sorted-map [0 5]  #{:x}
                              [5 10] #{:y}))
             (seq imap))
          "X MEETS Y, 2 separate segments are created as the end of an interval is exclusive")

    (t/is (nil? (get imap
                     -1)))

    (t/is (= #{:x}
             (get imap
                  0)
             (get imap
                  4)))

    (t/is (= #{:y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))

    (t/is (nil? (get imap
                     15)))

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 0
                                         3
                                         :x)
                      (interval.map/mark 5
                                         10
                                         :y)))
             (seq (interval.map/erase imap
                                      3
                                      8
                                      :x)))
          "Erasing X across contiguous segments, non-containing segments left intact")))



(t/deftest mark-meet-defrag

  (let [imap (interval.map/mark interval.map/empty
                                5
                                10
                                :x)]
    (t/is (= (seq imap)
             (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         8
                                         :x)
                      (interval.map/mark 8
                                         10
                                         :x)))
             (seq (-> interval.map/empty
                      (interval.map/mark 8
                                         10
                                         :x)
                      (interval.map/mark 5
                                         8
                                         :x))))
          "MEETS or INV MEETS"))

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 0
                                       10
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 0
                                       5
                                       :y)
                    (interval.map/mark 5
                                       10
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/mark 0
                                       5
                                       :y))))
        "MEETS or INV MEETS (> 1 value")

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   0
                                   15
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :x))))
        "MEETS, left merge")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       15
                                       :x)
                    (interval.map/mark 0
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       15
                                       :x)
                    (interval.map/mark 0
                                       5
                                       :y)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 5
                                       10
                                       :y))))
        "MEETS, left merge (> 1 value)"))



(t/deftest mark-rest-meet

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   0
                                   15
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :x)
                    (interval.map/mark 3
                                       10
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       12
                                       :x)
                    (interval.map/mark 3
                                       15
                                       :x))))
        "Merge"))



(t/deftest mark-equal

  ;; X = Y imap

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :x)
                 (interval.map/mark 5
                                    10
                                    :y))]

    (t/is (= (sorted-map [5 10]
                         #{:x
                           :y})
             imap)
          "X = Y, 1 segment is created and updated")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))

    (t/is (nil? (get imap
                     15)))

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     5
                                     10
                                     :y))
             (seq (interval.map/erase imap
                                      5
                                      10
                                      :x))
             (seq (interval.map/erase imap
                                      0
                                      10
                                      :x))
             (seq (interval.map/erase imap
                                      nil
                                      10
                                      :x))
             (seq (interval.map/erase imap
                                      nil
                                      15
                                      :x))
             (seq (interval.map/erase imap
                                      nil
                                      nil
                                      :x)))
          "Completing erasing a value from a segment, other values remains")

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         7
                                         :x)
                      (interval.map/mark 5
                                         10
                                         :y)))
             (seq (interval.map/erase imap
                                      7
                                      10
                                      :x))
             (seq (interval.map/erase imap
                                      7
                                      15
                                      :x))
             (seq (interval.map/erase imap
                                      7
                                      nil
                                      :x)))
          "Partially erasing a value from a segment, other values remains")))



(t/deftest mark-equal-defrag

  (let [imap (interval.map/mark interval.map/empty
                                5
                                10
                                :x)]

    (t/is (= (seq imap)
             (seq (interval.map/mark imap
                                     5
                                     10
                                     :x))
             (seq (interval.map/mark imap
                                     5
                                     8
                                     :x))
             (seq (interval.map/mark imap
                                     8
                                     10
                                     :x))
             (seq (interval.map/mark imap
                                     6
                                     8
                                     :x)))
          "Contains target value within, no need to fragment")))



(t/deftest mark-rest-equal

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   5
                                   20
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 7
                                       20
                                       :x))))
        "Merge")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       15
                                       :x)
                    (interval.map/mark 5
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       15
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/mark 7
                                       15
                                       :y))))
        "Merge (> 1 value)"))



(t/deftest mark-start

  ;; X STARTS Y and vice-versa, in various flavors

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 5
                                    8
                                    :x))]

    (t/is (= (seq (sorted-map [5 8]  #{:x
                                       :y}
                              [8 10] #{:y}))
             (seq imap))
          "X STARTS Y, existing segment is split in 2, left subsegment updated")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  7)))

    (t/is (= #{:y}
             (get imap
                  8)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))

    (t/is (nil? (get imap
                     15)))

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     5
                                     8
                                     :x))
             (seq (interval.map/erase imap
                                      5
                                      10
                                      :y))
             (seq (interval.map/erase imap
                                      0
                                      10
                                      :y))
             (seq (interval.map/erase imap
                                      nil
                                      10
                                      :y))
             (seq (interval.map/erase imap
                                      nil
                                      15
                                      :y))
             (seq (interval.map/erase imap
                                      nil
                                      nil
                                      :y)))
          "Erasing value from two adjacent segments, leaving other values intact"))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                     nil
                                     :y)
                 (interval.map/mark 5
                                    10
                                    :x))]

    (t/is (= (seq (sorted-map [5 10]   #{:x
                                         :y}
                              [10 nil] #{:y}))
             (seq imap))
          "X STARTS Y which is half-open at the end, existing segment is split in 2, left subsegment updated")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (= #{:y}
             (get imap
                  10)
             (get imap
                  15)))

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         7
                                         :y)
                      (interval.map/mark 5
                                         10
                                         :x)))
             (seq (interval.map/erase imap
                                      7
                                      nil
                                      :y)))
          "Partially erasing to the end value with half-open at the end interval")

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         7
                                         :y)
                      (interval.map/mark 5
                                         10
                                         :x)
                      (interval.map/mark 100
                                         nil
                                         :y)))
             (seq (interval.map/erase imap
                                      7
                                      100
                                      :y)))
          "Partially erasing value with half-open at the end interval"))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 5
                                    15
                                    :x))]

    (t/is (= (seq (sorted-map [5 10]  #{:x
                                        :y}
                              [10 15] #{:x}))
             (seq imap))
          "Y STARTS X, existing segment updated, 1 created beyond for X")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))
    
    (t/is (= #{:x}
             (get imap
                  10)
             (get imap
                  14)))

    (t/is (nil? (get imap
                     15)))

    (t/is (nil? (get imap
                     20)))

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         8
                                         :x)
                      (interval.map/mark 5
                                         10
                                         :y)
                      (interval.map/mark 12
                                         15
                                         :x)))
             (seq (interval.map/erase imap
                                      8
                                      12
                                      :x)))
          "Erasing middle of interval for a value accross segments, other values remain intact"))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 5
                                    nil
                                    :x))]

    (t/is (= (seq (sorted-map [5 10]   #{:y
                                         :x}
                              [10 nil] #{:x}))
             (seq imap))
        "Y STARTS X which is half-open at the end, existing segment updated, 1 created beyond for X")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (= #{:x}
             (get imap
                  10)
             (get imap
                  1000)))))



(t/deftest mark-start-defrag

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :y)
                    (interval.map/mark 0
                                       7
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :y)
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 5
                                       7
                                       :x))))
        "STARTS, left merge"))



(t/deftest mark-rest-start

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       13
                                       :x)
                    (interval.map/mark 5
                                       13
                                       :y)
                    (interval.map/mark 13
                                       15
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       15
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/mark 5
                                       13
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       15
                                       :x)
                    (interval.map/mark 7
                                       10
                                       :y)
                    (interval.map/mark 5
                                       13
                                       :y))))
        "Merge"))



(t/deftest mark-finish


  ;; X FINISHES Y and vice-versa, in various flavors

  ;; Should be fine since marking is associative.

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 8
                                    10
                                    :x))]

    (t/is (= (seq (sorted-map [5 8]  #{:y}
                              [8 10] #{:x
                                       :y}))
             (seq imap))
          "X FINISHES Y, existing segment split in 2, right subsegment updated")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:y}
             (get imap
                  5)
             (get imap
                  7)))

    (t/is (= #{:x
               :y}
             (get imap
                  8)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))
    
    (t/is (nil? (get imap
                     15))))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 2
                                    10
                                    :x))]

    (t/is (= (seq (sorted-map [2 5]  #{:x}
                              [5 10] #{:x
                                       :y}))
             (seq imap))
          "Y FINISHES X, existing segment updated, 1 created before for X")

    (t/is (nil? (get imap
                     1)))

    (t/is (= #{:x}
             (get imap
                  2)
             (get imap
                  4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))

    (t/is (nil? (get imap
                     15))))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark nil
                                    10
                                    :x))]

    (t/is (= (seq (sorted-map [nil 5] #{:x}
                              [5 10]  #{:y
                                        :x}))
             (seq imap))
          "Y FINISHES X which is half-open at start, existing segment updated, 1 created before for X")

    (t/is (= #{:x}
             (get imap
                  -10)
             (get imap
                  0)
             (get imap
                  4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))
    
    (t/is (nil? (get imap
                     15)))))



(t/deftest mark-finish-defrag

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       15
                                       :x)
                    (interval.map/mark 5
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 10
                                       15
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y))))
        "FINISHES, right merge")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       15
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)))

           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 5
                                       15
                                       :x))))
        "INV FINISHES, left merge"))



(t/deftest mark-during

  ;; X during Y and vice versa

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 2
                                    12
                                    :x))]

    (t/is (= (seq (sorted-map [2 5]   #{:x}
                              [5 10]  #{:y
                                        :x}
                              [10 12] #{:x}))
             (seq imap))
          "Y DURING X, existing segment update, 1 created before for X, 1 beyond for X")

    (t/is (nil? (get imap
                     1)))

    (t/is (= #{:x}
             (get imap
                  2)
             (get imap
                  4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (= #{:x}
             (get imap
                  10)
             (get imap
                  11)))

    (t/is (nil? (get imap
                     12)))
    
    (t/is (nil? (get imap
                     15)))

    (t/is (= (seq (interval.map/mark interval.map/empty
                                     5
                                     10
                                     :y))
             (seq (interval.map/erase imap
                                      2
                                      12
                                      :x)))
          "Removing outer values while inner one remain intact")

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 2
                                         4
                                         :x)
                      (interval.map/mark 5
                                         10
                                         :y)
                      (interval.map/mark 11
                                         12
                                         :x)))
             (seq (interval.map/erase imap
                                      4
                                      11
                                      :x)))
          "Removing middle of value around an inner value"))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 6
                                    8
                                    :x))]

    (t/is (= (seq (sorted-map [5 6]  #{:y}
                              [6 8]  #{:y
                                       :x}
                              [8 10] #{:y}))
             (seq imap))
          "X DURING Y, existing segment split in 3, middle subsegment updated")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:y}
             (get imap
                  5)
             (get imap
                  5.5)))

    (t/is (= #{:x
               :y}
             (get imap
                  6)
             (get imap
                  7)))

    (t/is (= #{:y}
             (get imap
                  8)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))
    
    (t/is (nil? (get imap
                     15)))))



(t/deftest mark-overlap

  ;; X OVERLAPS Y and vice-versa

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 2
                                    7
                                    :x))]

    (t/is (= (seq (sorted-map [2 5]  #{:x}
                              [5 7]  #{:x
                                       :y}
                              [7 10] #{:y}))
             (seq imap))
          "X OVERLAPS Y, existing segment split in 2, left subsegment udpated, 1 segment created before for X")

    (t/is (nil? (get imap
                     1)))

    (t/is (= #{:x}
             (get imap
                  2)
             (get imap
                  4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  6)))

    (t/is (= #{:y}
             (get imap
                  7)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))

    (t/is (nil? (get imap
                     15))))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark nil
                                    7
                                    :x))]

    (t/is (= (seq (sorted-map [nil 5] #{:x}
                              [5 7]   #{:y
                                        :x}
                              [7 10]  #{:y}))
             (seq imap))
          "X OVERLAPS y and is half-open at start, existing segment split in 2, left one updated, 1 segment created before for X")

    (t/is (= #{:x}
             (get imap
                  -1)
             (get imap
                  0)
             (get imap
                  4)))

    (t/is (= #{:x
               :y}
             (get imap
                  5)
             (get imap
                  6)))

    (t/is (= #{:y}
             (get imap
                  7)
             (get imap
                  9)))

    (t/is (nil? (get imap
                     10)))
    
    (t/is (nil? (get imap
                     15))))


  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 8
                                    15
                                    :x))]

    (t/is (= (seq (sorted-map [5 8]   #{:y}
                              [8 10]  #{:y
                                        :x}
                              [10 15] #{:x}))
             (seq imap))
          "Y OVERLAPS X, existing segment split in 2, right subsegment updated, 1 segment created beyond for X")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:y}
             (get imap
                  5)
             (get imap
                  7)))

    (t/is (= #{:x
               :y}
             (get imap
                  8)
             (get imap
                  9)))

    (t/is (= #{:x}
             (get imap
                  10)
             (get imap
                  14)))

    (t/is (nil? (get imap
                     15)))

    (t/is (nil? (get imap
                     20)))))



(t/deftest mark-overlap-defrag

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   5
                                   10
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 8
                                       10
                                       :x)
                    (interval.map/mark 5
                                       9
                                       :x))))
        "OVERLAPS")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 10
                                       15
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 5
                                       12
                                       :x))))
        "OVERLAPS, merge")

  (t/is (= (seq (interval.map/mark interval.map/empty
                               0
                               15
                               :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :x)
                    (interval.map/mark 5
                                       12
                                       :x))))
        "OVERLAPS, extend existing segment + left merge")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       12
                                       :x)
                    (interval.map/mark 10
                                       12
                                       :y)
                    (interval.map/mark 10
                                       12
                                       :x)
                    (interval.map/mark 12
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 5
                                       12
                                       :x))))
        "OVERLAPS, left merge (> 1 value)")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :y)
                    (interval.map/mark 5
                                       15
                                       :x)
                    (interval.map/mark 5
                                       15
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       15
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 0
                                       10
                                       :y))))
        "OVERLAPS, right merge"))



(t/deftest mark-rest-overlap

  (t/is (= (seq (interval.map/mark interval.map/empty
                               5
                               20
                               :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 7
                                       17
                                       :x))))
        "OVERLAP, merge")

  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       7
                                       :x)
                    (interval.map/mark 7
                                       10
                                       :y)
                    (interval.map/mark 7
                                       10
                                       :x)
                    (interval.map/mark 10
                                       15
                                       :y)
                    (interval.map/mark 15
                                       17
                                       :x)
                    (interval.map/mark 15
                                       17
                                       :y)
                    (interval.map/mark 17
                                       20
                                       :x)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 7
                                       17
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 12
                                       14
                                       :y)
                    (interval.map/mark 7
                                       17
                                       :y))))
        "OVERLAP (> 1 value)"))



(t/deftest mark-rest

  ;; Updating several segments during one mark with a new value

  (let [imap (-> interval.map/empty
                 (interval.map/mark 10
                                    15
                                    :b)
                 (interval.map/mark 5
                                    10
                                    :a)
                 (interval.map/mark 20
                                    30
                                    :c)
                 (interval.map/mark 5
                                    35
                                    :d))]

    (t/is (= (seq (sorted-map [5 10]  #{:a
                                        :d}
                              [10 15] #{:b
                                        :d}
                              [15 20] #{:d}
                              [20 30] #{:c
                                        :d}
                              [30 35] #{:d}))
             (seq imap))
          "X spans several segments, all are updated")

    (t/is (nil? (get imap
                     4)))

    (t/is (= #{:a
               :d}
             (get imap
                  5)
             (get imap
                  9)))

    (t/is (= #{:b
               :d}
             (get imap
                  10)
             (get imap
                  14)))

    (t/is (= #{:d}
             (get imap
                  15)))

    (t/is (= #{:d}
             (get imap
                  19)))


    (t/is (= #{:c
               :d}
             (get imap
                  20)
             (get imap
                  29)))

    (t/is (= #{:d}
             (get imap
                  30)
             (get imap
                  34)))

    (t/is (nil? (get imap
                     35)))
    
    (t/is (nil? (get imap
                     50)))

    (t/is (= [[[10 15] #{:b
                         :d}]
              [[15 20] #{:d}]
              [[20 30] #{:c
                         :d}]]
             (subseq imap
                     >= 10
                     <  30)
             (subseq imap
                     >= 12
                     <= 25)
             (subseq imap
                     > 9
                     < 30))
          "Querying segments in ways that should be equivalent given the state of the imap")

    (t/is (= [[[20 30] #{:c
                         :d}]
              [[15 20] #{:d}]
              [[10 15] #{:b
                         :d}]]
             (rsubseq imap
                      >= 10
                      <  30)
             (rsubseq imap
                      >= 12
                      <= 25))
          "Reverse segment querying")

    (t/is (= (seq imap)
             (subseq imap
                     >= nil)
             (subseq imap
                     > nil))
          "Querying segments starting at nil")

    (t/is (= (reverse (seq imap))
             (rsubseq imap
                      >= nil)
             (rsubseq imap
                      > nil))
          "Reverse segment querying starting at nil")

    (t/is (= [[[20 30] #{:c
                         :d}]
              [[30 35] #{:d}]]
             (subseq imap
                     >= 20)
             (subseq imap
                     >= 25)
             (subseq imap
                     > 19))
          "Querying segments after a given point")

    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         10
                                         :a)
                      (interval.map/mark 10
                                         15
                                         :b)
                      (interval.map/mark 20
                                         30
                                         :c)))
             (seq (interval.map/erase imap
                                      5
                                      35
                                      :d)))
          "Removing values accross several segments, leaving other values intact")


    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         10
                                         :a)
                      (interval.map/mark 5
                                         9
                                         :d)
                      (interval.map/mark 10
                                         15
                                         :b)
                      (interval.map/mark 20
                                         30
                                         :c)
                      (interval.map/mark 32
                                         35
                                         :d)))
             (seq (interval.map/erase imap
                                      9
                                      32
                                      :d)))
          "Erasing middle of values accross several segments, leaving other values intact")))



(t/deftest mark-sparse-segments

  (t/is (= (seq (sorted-map [0 5]   #{:a
                                      :c}
                            [5 30]  #{:c}
                            [30 35] #{:b
                                      :c}))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       5
                                       :a)
                    (interval.map/mark 30
                                       35
                                       :b)
                    (interval.map/mark 0
                                       35
                                       :c))))
        "New segments are created between existing ones"))



(t/deftest union

  (let [imap (-> interval.map/empty
                 (interval.map/mark 0
                                    15
                                    :a)
                 (interval.map/mark 12
                                    nil
                                    :b)
                 (interval.map/mark 20
                                    25
                                    :c)
                 (interval.map/mark 30
                                    nil
                                    :d)
                 (interval.map/mark 35
                                    40
                                    :e)
                 (interval.map/mark nil
                                    0
                                    :f))]

    (t/is (= #{:b
               :d
               :e}
             (interval.map/union (subseq imap
                                         >= 26))
             (interval.map/union (subseq imap
                                         >= 26
                                         <  45))
             (interval.map/union (subseq imap
                                         >= 26
                                         <= 35)))
          "Equivalent unions given current state of imap")

    (t/is (= #{:b
               :d
               :e}
             (interval.map/union (rsubseq imap
                                          >= 26))
             (interval.map/union (rsubseq imap
                                          >= 26
                                          <  45))
             (interval.map/union (rsubseq imap
                                          >= 26
                                          <= 35)))
          "Using reverse segment querying does not change anything")

    (t/is (= #{:b
               :d}
             (interval.map/union (subseq imap
                                         >= 1000000))
             (interval.map/union (rsubseq imap
                                          >= 1000000)))
          "Union of values at intervals with half-open ends")

    (t/is (= #{:f}
             (interval.map/union (subseq imap
                                         < 0))
             (interval.map/union (rsubseq imap
                                          < 0)))
          "Union of values at intervals with half-open starts")))



(t/deftest mark-rest-defrag

  ;; Additional defragmentation tests when marking over several segments.

  (let [imap (-> interval.map/empty
                 (interval.map/mark 0
                                    20
                                    :x)
                 (interval.map/mark 5
                                    10
                                    :y)
                 (interval.map/mark 15
                                    20
                                    :y))]
    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 0
                                         20
                                         :x)
                      (interval.map/mark 5
                                         20
                                         :y)))
             (seq (interval.map/mark imap
                                     10
                                     15
                                     :y))
             (seq (interval.map/mark imap
                                     7
                                     15
                                     :y))
             (seq (interval.map/mark imap
                                     5
                                     15
                                     :y))
             (seq (interval.map/mark imap
                                     5
                                     17
                                     :y))
             (seq (interval.map/mark imap
                                     5
                                     20
                                     :y))
             (seq (interval.map/mark imap
                                     7
                                     17
                                     :y))
             (seq (interval.map/mark imap
                                     7
                                     20
                                     :y)))
          "Adjacent segments"))

  (let [imap (-> interval.map/empty
                 (interval.map/mark 5
                                    10
                                    :x)
                 (interval.map/mark 15
                                    30
                                    :x)
                 (interval.map/mark 15
                                    20
                                    :y))]
    (t/is (= (seq (-> interval.map/empty
                      (interval.map/mark 5
                                         30
                                         :x)
                      (interval.map/mark 15
                                         20
                                         :y)))
             (seq (interval.map/mark imap
                                     5
                                     15
                                     :x))
             (seq (interval.map/mark imap
                                     5
                                     17
                                     :x))
             (seq (interval.map/mark imap
                                     5
                                     20
                                     :x))
             (seq (interval.map/mark imap
                                     5
                                     25
                                     :x))
             (seq (interval.map/mark imap
                                     5
                                     30
                                     :x))
             (seq (interval.map/mark imap
                                     7
                                     15
                                     :x))
             (seq (interval.map/mark imap
                                     7
                                     17
                                     :x))
             (seq (interval.map/mark imap
                                     7
                                     20
                                     :x))
             (seq (interval.map/mark imap
                                     7
                                     25
                                     :x))
             (seq (interval.map/mark imap
                                     7
                                     30
                                     :x)))
          "Non-adjacent segments")))



(t/deftest erase-defrag

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   0
                                   10
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/erase 5
                                        10
                                        :y))))
        "Left merge")

  (t/is (= (seq (interval.map/mark interval.map/empty
                                   0
                                   10
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 0
                                       5
                                       :y)
                    (interval.map/erase 0
                                        5
                                        :y))))
        "Right merge")


  (t/is (= (seq (interval.map/mark interval.map/empty
                                   0
                                   10
                                   :x))
           (seq (-> interval.map/empty
                    (interval.map/mark 0
                                       10
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/erase 5
                                        10
                                        :y))))
        "Merge")

  (let [imap (-> interval.map/empty
                 (interval.map/mark 0
                                    10
                                    :x)
                 (interval.map/mark 5
                                    10
                                    :y))]
    (t/is (= (seq (interval.map/mark interval.map/empty
                                     0
                                     10
                                     :x))
             (seq (interval.map/erase imap
                                      0
                                      10
                                      :y))
             (seq (interval.map/erase imap
                                      nil
                                      10
                                      :y))
             (seq (interval.map/erase imap
                                      3
                                      10
                                      :y))
             (seq (interval.map/erase imap
                                      0
                                      15
                                      :y))
             (seq (interval.map/erase imap
                                      0
                                      nil
                                      :y)))
          "Merge left (multi-segment)")

    (let [imap-2 (interval.map/mark imap
                                    2
                                    3
                                    :y)]
      (t/is (= (seq (interval.map/mark interval.map/empty
                                       0
                                       10
                                       :x))
               (seq (interval.map/erase imap-2
                                        0
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        0
                                        15
                                        :y))
               (seq (interval.map/erase imap-2
                                        nil
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        nil
                                        15
                                        :y))
               (seq (interval.map/erase imap-2
                                        1
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        1
                                        15
                                        :y))
               (seq (interval.map/erase imap-2
                                        1
                                        nil
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        15
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        nil
                                        :y)))
            "Merge left (multi-segment)"))

    (let [imap-2 (-> imap
                     (interval.map/mark 2
                                        3
                                        :y)
                     (interval.map/mark 10
                                        20
                                        :x))]
      (t/is (= (seq (interval.map/mark interval.map/empty
                                       0
                                       20
                                       :x))
               (seq (interval.map/erase imap-2
                                        0
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        0
                                        15
                                        :y))
               (seq (interval.map/erase imap-2
                                        0
                                        20
                                        :y))
               (seq (interval.map/erase imap-2
                                        0
                                        25
                                        :y))
               (seq (interval.map/erase imap-2
                                        0
                                        nil
                                        :y))
               (seq (interval.map/erase imap-2
                                        nil
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        nil
                                        15
                                        :y));
               (seq (interval.map/erase imap-2
                                        nil
                                        20
                                        :y));
               (seq (interval.map/erase imap-2
                                        nil
                                        25
                                        :y))
               (seq (interval.map/erase imap-2
                                        nil
                                        nil
                                        :y))
               (seq (interval.map/erase imap-2
                                        1
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        1
                                        15
                                        :y));
               (seq (interval.map/erase imap-2
                                        1
                                        20
                                        :y));
               (seq (interval.map/erase imap-2
                                        1
                                        25
                                        :y))
               (seq (interval.map/erase imap-2
                                        1
                                        nil
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        10
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        15
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        20
                                        :y))
               (seq (interval.map/erase imap-2
                                        2
                                        25
                                        :y))

               (seq (interval.map/erase imap-2
                                        2
                                        nil
                                        :y)))
            "Merge (multi-segment, recurring further)")))


  (t/is (= (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       7
                                       :y)
                    (interval.map/mark 5
                                       7
                                       :x)
                    (interval.map/mark 7
                                       10
                                       :y)
                    (interval.map/mark 15
                                       17
                                       :y)
                    (interval.map/mark 17
                                       20
                                       :x)
                    (interval.map/mark 17
                                       20
                                       :y)))
           (seq (-> interval.map/empty
                    (interval.map/mark 5
                                       10
                                       :x)
                    (interval.map/mark 5
                                       10
                                       :y)
                    (interval.map/mark 15
                                       20
                                       :x)
                    (interval.map/mark 15
                                       20
                                       :y)
                    (interval.map/erase 7
                                        17
                                        :x))))
        "Gap in between segments, acc is re-assoced"))
