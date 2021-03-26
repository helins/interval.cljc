;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns dvlopt.interval.set-test

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test        :as t]
            [dvlopt.interval.set :as interval.set]))


;;;;;;;;;;


(t/deftest erase

  (let [iset (interval.set/mark interval.set/empty
                                5
                                10)]
    (t/is (nil? (seq (interval.set/erase iset
                                         5
                                         10)))
          "Mono-segment interval")

    (t/is (= (seq (interval.set/mark interval.set/empty
                                     7
                                     10))
             (seq (interval.set/erase iset
                                      5
                                      7)))
          "STARTS")

    (t/is (= (seq (interval.set/mark interval.set/empty
                                     5
                                     7))
             (seq (interval.set/erase iset
                                      7
                                      10)))
          "ENDS")

    (t/is (= (seq (-> interval.set/empty
                      (interval.set/mark 5
                                         7)
                      (interval.set/mark 9
                                         10)))
             (seq (interval.set/erase iset
                                      7
                                      9)))
          "DURING")

    (t/is (nil? (seq (-> interval.set/empty
                         (interval.set/mark 5
                                            10)
                         (interval.set/mark 20
                                            30)
                         (interval.set/mark 40
                                            45)
                         (interval.set/erase 5
                                             nil))))
          "Across several segments")

    (t/is (= (seq (interval.set/mark interval.set/empty
                                     17
                                     20))
             (seq (-> interval.set/empty
                      (interval.set/mark 5
                                         10)
                      (interval.set/mark 15
                                         20)
                      (interval.set/erase 5
                                          17))))
          "STARTS (multi-segment)")))




(t/deftest mark

  (t/is (= (seq (sorted-set [5 10]))
           (seq (interval.set/mark interval.set/empty
                                   5
                                   10)))
        "Adding first interval")

  (let [iset (interval.set/mark interval.set/empty
                                10
                                15)]
    (t/is (= (seq iset)
             (seq (interval.set/mark iset
                                     10
                                     15))
             (seq (interval.set/mark iset
                                     10
                                     13))
             (seq (interval.set/mark iset
                                     12
                                     14))
             (seq (interval.set/mark iset
                                     12
                                     15)))
          "Within a single existing segment"))

  (let [iset (-> interval.set/empty
                 (interval.set/mark 5
                                    10)
                 (interval.set/mark 15
                                    20))]
    (t/is (= (seq (sorted-set [5 10]
                              [15 20]))
             (seq iset))
          "Adding two disjoint intervals")
    (t/is (false? (contains? iset
                             4)))
    (t/is (contains? iset
                     5))
    (t/is (contains? iset
                     7))
    (t/is (contains? iset
                     9))
    (t/is (false? (contains? iset
                             10)))
    (t/is (false? (contains? iset
                             14)))
    (t/is (contains? iset
                     15))
    (t/is (contains? iset
                     17))
    (t/is (contains? iset
                     19))
    (t/is (false? (contains? iset
                             20))))

  (t/is (= (seq (sorted-set [5 15]))
           (seq (-> interval.set/empty
                    (interval.set/mark 5
                                       10)
                    (interval.set/mark 10
                                       15)))
           (seq (-> interval.set/empty
                    (interval.set/mark 10
                                       15)
                    (interval.set/mark 5
                                       10)))
           (seq (-> interval.set/empty
                    (interval.set/mark 5
                                       7)
                    (interval.set/mark 10
                                       15)
                    (interval.set/mark 7
                                       10))))
        "Adjacent segments are merged together")

  (t/is (= (seq (sorted-set [0 50]))
           (seq (-> interval.set/empty
                    (interval.set/mark 5
                                       10)
                    (interval.set/mark 15
                                       20)
                    (interval.set/mark 30
                                       40)
                    (interval.set/mark 0
                                       50))))
        "Merging several segments")

  (t/is (= (seq (sorted-set [0 nil]))
           (seq (-> interval.set/empty
                    (interval.set/mark 0
                                       5)
                    (interval.set/mark 10
                                       15)
                    (interval.set/mark 5
                                       nil))))
        "Merging segments towards +Infinity")

  (t/is (= (seq (sorted-set [nil 50]))
           (seq (-> interval.set/empty
                    (interval.set/mark 10
                                       15)
                    (interval.set/mark 20
                                       30)
                    (interval.set/mark nil
                                       50))))
        "Merging segments towards -Infinity")

 (t/is (= (seq (sorted-set [0 5]
                            [6 50]))
           (seq (-> interval.set/empty
                    (interval.set/mark 0
                                       5)
                    (interval.set/mark 25
                                       30)
                    (interval.set/mark 35
                                       40)
                    (interval.set/mark 6
                                       50))))
        "Not merging disjoint left segment"))
