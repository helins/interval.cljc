;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.interval.util

  ""
  
  {:author "Adam Helinski"
   :no-doc true})


;;;;;;;;;; Mainly point comparisons


(defn cmp

  ;; Compare `a` and `b`, returning true if `a` is greater.
  ;;
  ;; Accounts for infinity (ie. nil).

  [a b]

  (if (nil? a)
    true
    (if (nil? b)
      false
      (if (number? a)
        (if (number? b)
          (< a
             b)
          (if-some [b-2 (first b)]
            (< a
               b-2)
            false))
        (if-some [a-2 (second a)]
          (if (number? b)
            (<= a-2
                b)
            (if-some [b-2 (first b)]
              (<= a-2
                  b-2)
              false))
          false)))))



(defn disjoint?

  ;; Are these two intervals disjoint?

  [to from]

  (and (some? to)
       (some? from)
       (< to
          from)))



(defn overlapping?

  ;; Are these two intervals overlapping?

  [to from]

  (or (nil? to)
      (nil? from)
      (> to
         from)))



(defn point<-

  ;; Is point `a` stricly lesser than point `b`?
  ;;
  ;; Handles nil as -Infinity.

  [a b]

  (if (nil? a)
    (some? b)
    (and (some? b)
         (< a
            b))))



(defn point<=-

  ;; Is point `a` lesser than or equal to point `b`?
  ;;
  ;; Handles nil as -Infinity.

  [a b]

  (or (= a
         b)
      (nil? a)
      (and (some? b)
           (<= a
               b))))



(defn point<+

  ;; Is point `a` stricly lesser than point `b`?
  ;;
  ;; Handles nil as +Infinity.

  [a b]

  (if (nil? b)
    (some? a)
    (and (some? a)
         (< a
            b))))



(defn point<=+

  ;; Is point `a` lesser than or equal to point `b`?
  ;;
  ;; Handles nil as +Infinity.

  [a b]

  (or (= a
         b)
      (nil? b)
      (and (some? a)
           (<= a
               b))))
