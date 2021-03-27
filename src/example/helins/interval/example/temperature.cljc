;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.interval.example.temperature

  ""

  (:require [helins.interval.set :as iset]))


;;;;;;;;;;


(def too-hot

  "Simulating recording temperature above some threshold.
  
   Intervals represents hours in a day."

  (-> iset/empty
      (iset/mark 11 13)
      (iset/mark 14 16)
      (iset/erase 15 20)))



(comment


  ;; Was it too hot at noon?
  ;; Yes, an interval proofs that.
  ;;
  (= (get too-hot
          12)
     [11 13])


  ;; Was it too hot at hour 16?
  ;; No, no interval found.
  ;;
  (nil? (get too-hot
             16))


  ;; Segments:
  ;;
  (= (seq too-hot)

     (list [11 13]
           [14 15]))


  ;; What happened before noon?
  ;;
  (= (subseq too-hot
             >= 0
             <= 12)
     (list [11 13]))


  )
