(ns dvlopt.interval-test

  ""

  ;; Cf. https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

  {:author "Adam Helinski"}

  (:require [clojure.core    :as clj]
            [clojure.test    :as t]
            [dvlopt.interval :as interval])
  ;;
  ;; <!> Attention, highly confusing when not kept in mind <!>
  ;;
  (:refer-clojure :exclude [assoc]))


;;;;;;;;;;


(t/deftest assoc

  (t/is (= (sorted-map [5 10]
                       #{:x})
           (interval/assoc (interval/tree)
                           5
                           10
                           :x))
        "Empty tree, 1 segment is created representing the first interval")

  (t/is (= (sorted-map [5 10]  #{:x}
                       [30 35] #{:y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :x)
               (interval/assoc 30
                               35
                               :y)))
        "X < Y, 2 separate segments are created")

  (t/is (= (sorted-map [0 5]  #{:x}
                       [5 10] #{:y})
           (-> (interval/tree)
               (interval/assoc 0
                               5
                               :x)
               (interval/assoc 5
                               10
                               :y)))
        "X MEETS Y, 2 separate segments are created as the end of an interval is exclusive")

  (t/is (= (sorted-map [5 10]
                       #{:x :y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :x)
               (interval/assoc 5
                               10
                               :y)))
        "X = Y, 1 segment is created and updated")

  (t/is (= (sorted-map [5 8]  #{:x
                                :y}
                       [8 10] #{:y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 5
                               8
                               :x)))
        "X STARTS Y, existing segment is split in 2, left subsegment updated")

  (t/is (= (sorted-map [5 10]   #{:y
                                  :x}
                       [10 nil] #{:y})
           (-> (interval/tree)
               (interval/assoc 5
                               nil
                               :y)
               (interval/assoc 5
                               10
                               :x)))
        "X STARTS Y which is half-open at the end, existing segment is split in 2, left subsegment updated")

  (t/is (= (sorted-map [5 10]  #{:y
                                 :x}
                       [10 15] #{:x})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 5
                               15
                               :x)))
        "Y STARS X, existing segment updated, 1 created beyond for X")

  (t/is (= (sorted-map [5 10]   #{:y
                                  :x}
                       [10 nil] #{:x})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 5
                               nil
                               :x)))
        "Y STARTS X which is half-open at the end, existing segment updated, 1 created beyond for X")

  (t/is (= (sorted-map [2 5]  #{:x}
                       [5 10] #{:y
                                :x})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 2
                               10
                               :x)))
        "Y FINISHES X, existing segment updated, 1 created before for X")

  (t/is (= (sorted-map [nil 5] #{:x}
                       [5 10]  #{:y
                                 :x})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc nil
                               10
                               :x)))
        "Y FINISHES X which is half-open at start, existing segment updated, 1 created before for X")

  (t/is (= (sorted-map [2 5]  #{:x}
                       [5 7]  #{:y
                                :x}
                       [7 10] #{:y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 2
                               7
                               :x)))
        "X OVERLAPS Y, existing segment split in 2, left subsegment udpated, 1 segment created before for X")

  (t/is (= (sorted-map [nil 5]  #{:x}
                       [5 7]    #{:y
                                  :x}
                       [7 10]   #{:y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc nil
                               7
                               :x)))
        "X OVERLAPS y and is half-open at start, existing segment split in 2, left one updated, 1 segment created before for X")

  (t/is (= (sorted-map [2 5]   #{:x}
                       [5 10]  #{:y
                                 :x}
                       [10 12] #{:x})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 2
                               12
                               :x)))
        "Y DURING X, existing segment update, 1 created before for X, 1 beyond for X")

  (t/is (= (sorted-map [5 8]  #{:y}
                       [8 10] #{:x
                                :y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 8
                               10
                               :x)))
        "X FINISHES Y, existing segment split in 2, right subsegment updated")

  (t/is (= (sorted-map [5 6]  #{:y}
                       [6 8]  #{:y
                                :x}
                       [8 10] #{:y})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 6
                               8
                               :x)))
        "X DURING Y, existing segment split in 3, middle subsegment updated")

  (t/is (= (sorted-map [5 8]   #{:y}
                       [8 10]  #{:y
                                 :x}
                       [10 15] #{:x})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :y)
               (interval/assoc 8
                               15
                               :x)))
        "Y OVERLAPS X, existing segment split in 2, right subsegment updated, 1 segment created beyond for X")

  (t/is (= (sorted-map [5 10]  #{:a
                                 :d}
                       [10 15] #{:b
                                 :d}
                       [20 30] #{:c
                                 :d}
                       [30 35] #{:d})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 10
                               15
                               :b)
               (interval/assoc 20
                               30
                               :c)
               (interval/assoc 5
                               35
                               :d)))
        "X spans several segments, all are updated"))
