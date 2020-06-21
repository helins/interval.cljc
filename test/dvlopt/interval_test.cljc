(ns dvlopt.interval-test

  ""

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
                       #{:a})
           (interval/assoc (interval/tree)
                           5
                           10
                           :a))
        "Associng to an empty tree creates just one full segment")

  (t/is (= (sorted-map [0 5]  #{:b}
                       [5 10] #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 0
                               5
                               :b)))
        "When no overlapping exists, a full one is created")

  (t/is (= (sorted-map [5 10]
                       #{:a :b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 5
                               10
                               :b)))
        "Associng an interval that matches an existing segment merely conj to this segment")

  (t/is (= (sorted-map [5 8]  #{:a
                                :b}
                       [8 10] #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 5
                               8
                               :b)))
        "Associng an interval ending before an existing segment splits that segment in two")

  (t/is (= (sorted-map [5 10]   #{:a
                                  :b}
                       [10 nil] #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               nil
                               :a)
               (interval/assoc 5
                               10
                               :b)))
        "Associng a closed interval starting at an existing half-open end segment splits that segment in two")

  (t/is (= (sorted-map [5 10]  #{:a
                                 :b}
                       [10 15] #{:b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 5
                               15
                               :b)))
        "Associng an interval ending after an existing segment creates a new one beyond")

  (t/is (= (sorted-map [5 10]   #{:a
                                  :b}
                       [10 nil] #{:b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 5
                               nil
                               :b)))
        "Associng a half-open  end interval starting at existing segment creates a new one")

  (t/is (= (sorted-map [2 5]  #{:b}
                       [5 10] #{:a
                                :b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 2
                               10
                               :b)))
        "Associng an interval starting before an existing segment creates a new one before")

  (t/is (= (sorted-map [nil 5] #{:b}
                       [5 10]  #{:a
                                 :b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc nil
                               10
                               :b)))
        "Associng an half-open start interval ending at an existing finite segment creates a new one before")

  (t/is (= (sorted-map [2 5]  #{:b}
                       [5 7]  #{:a
                                :b}
                       [7 10] #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 2
                               7
                               :b)))
        "Associng an interval starting before an existing segment but ending before it create a new one before
         and splits the existing one")

  (t/is (= (sorted-map [nil 5]  #{:b}
                       [5 7]    #{:a
                                  :b}
                       [7 10]   #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc nil
                               7
                               :b)))
        "Associng a half-open start interval ending before an existing segment creates a new one before
         and splits the existing one")

  (t/is (= (sorted-map [2 5]   #{:b}
                       [5 10]  #{:a
                                 :b}
                       [10 12] #{:b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 2
                               12
                               :b)))
        "Associng an interval starting before and ending after an existing segment creates indeed two additional segments")

  (t/is (= (sorted-map [5 8]  #{:a
                                :b}
                       [8 10] #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 5
                               8
                               :b)))
        "Associng an interval starting after an existing segment splits that segment in two")

  (t/is (= (sorted-map [5 6]  #{:a}
                       [6 8]  #{:a
                                :b}
                       [8 10] #{:a})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 6
                               8
                               :b)))
        "Associng an interval within an existing segment splits that segment in three")

  (t/is (= (sorted-map [5 8]   #{:a}
                       [8 10]  #{:a
                                 :b}
                       [10 15] #{:b})
           (-> (interval/tree)
               (interval/assoc 5
                               10
                               :a)
               (interval/assoc 8
                               15
                               :b)))
        "Associng an interval start after an existing segment and ending after that segment splits it in two and creates a new one beyong")

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
        "Associng an interval spanning several segments updates all segments and beyond when needed"))
