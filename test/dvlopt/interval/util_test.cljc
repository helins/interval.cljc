;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns dvlopt.interval.util-test

  ""

  {:author "Adam Helinski"}

  (:require [clojure.test         :as t]
            [dvlopt.interval.util :as interval.util]))


;;;;;;;;;;


(t/deftest cmp

  (t/is (interval.util/cmp nil
                           nil))
  (t/is (interval.util/cmp nil
                           5))
  (t/is (interval.util/cmp nil
                           [0 10]))
  (t/is (false? (interval.util/cmp 5
                                   nil)))
  (t/is (false? (interval.util/cmp [0 10]
                                   nil)))
  (t/is (false? (interval.util/cmp 5
                                   5)))
  (t/is (interval.util/cmp 5
                           6))
  (t/is (false? (interval.util/cmp 6
                                   5)))
  (t/is (interval.util/cmp 5
                           [6 10]))
  (t/is (false? (interval.util/cmp 5
                                   [5 10])))
  (t/is (false? (interval.util/cmp 5
                                   [nil 10])))
  (t/is (interval.util/cmp [0 5]
                           [5 10]))
  (t/is (interval.util/cmp [0 5]
                           [6 10]))
  (t/is (false? (interval.util/cmp [5 nil]
                                   [10 #__]))))



(t/deftest disjoint?

  (t/is (interval.util/disjoint? 4
                                 5))
  (t/is (false? (interval.util/disjoint? 5
                                         5)))

  (t/is (false? (interval.util/disjoint? 6
                                         5)))
  (t/is (false? (interval.util/disjoint? nil
                                         10)))
  (t/is (false? (interval.util/disjoint? 5
                                         nil))))



(t/deftest overlapping?

  (t/is (interval.util/overlapping? 6
                                    5))
  (t/is (false? (interval.util/overlapping? 5
                                            5)))
  (t/is (false? (interval.util/overlapping? 4
                                            5)))
  (t/is (interval.util/overlapping? nil
                                    5))
  (t/is (interval.util/overlapping? 5
                                    nil)))



(t/deftest point<-

  (t/is (interval.util/point<- 4
                               5))
  (t/is (false? (interval.util/point<- 5
                                       5)))
  (t/is (false? (interval.util/point<- 6
                                       5)))
  (t/is (false? (interval.util/point<- nil
                                       nil)))
  (t/is (interval.util/point<- nil
                               5))
  (t/is (false? (interval.util/point<- 5
                                       nil))))



(t/deftest point<=-

  (t/is (interval.util/point<=- 4
                                5))
  (t/is (interval.util/point<=- 5
                                5))
  (t/is (false? (interval.util/point<=- 6
                                        5)))
  (t/is (interval.util/point<=- nil
                                nil))
  (t/is (interval.util/point<=- nil
                                5))
  (t/is (false? (interval.util/point<=- 5
                                        nil))))



(t/deftest point<+

  (t/is (interval.util/point<+ 4
                               5))
  (t/is (false? (interval.util/point<+ 5
                                       5)))
  (t/is (false? (interval.util/point<+ 6
                                       5)))
  (t/is (false? (interval.util/point<+ nil
                                       nil)))
  (t/is (false? (interval.util/point<+ nil
                                       5)))
  (t/is (interval.util/point<+ 5
                               nil)))



(t/deftest point<=+

  (t/is (interval.util/point<=+ 4
                                5))
  (t/is (interval.util/point<=+ 5
                                5))
  (t/is (false? (interval.util/point<=+ 6
                                        5)))
  (t/is (interval.util/point<=+ nil
                                nil))
  (t/is (false? (interval.util/point<=+ nil
                                        5)))
  (t/is (interval.util/point<=+ 5
                                nil)))
