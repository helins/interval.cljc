(ns dvlopt.interval.util

  ""
  
  {:author "Adam Helinski"
   :no-doc true})


;;;;;;;;;; Mainly point comparisons


(defn cmp

  ""

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

  ""

  [to from]

  (and (some? to)
       (some? from)
       (< to
          from)))



(defn overlapping?

  ;;

  [to from]

  (or (nil? to)
      (nil? from)
      (> to
         from)))



(defn point<-

  ;; Handles nil as -Infinity.

  [a b]

  (if (nil? a)
    (some? b)
    (and (some? b)
         (< a
            b))))



(defn point<=-

  ;;

  [a b]

  (or (= a
         b)
      (nil? a)
      (and (some? b)
           (<= a
               b))))



(defn point<+

  ;; Handles nil as +Infinity.

  [a b]

  (if (nil? b)
    (some? a)
    (and (some? a)
         (< a
            b))))



(defn point<=+

  ;;

  [a b]

  (or (= a
         b)
      (nil? b)
      (and (some? a)
           (<= a
               b))))
