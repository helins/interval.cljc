(ns dvlopt.interval.set-test

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test        :as t]
            [dvlopt.interval.set :as interval.set]))


;;;;;;;;;;


(t/deftest mark

  (t/is (= (seq (sorted-set [5 10]))
           (seq (interval.set/mark interval.set/empty
                                   5
                                   10)))
        "Adding first interval")

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
