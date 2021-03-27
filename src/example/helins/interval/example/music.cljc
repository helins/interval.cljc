;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.interval.example.music

  ""

  (:require [helins.interval.map  :as imap]))


;;;;;;;;;;


(def music

  "A piece music.

   Those intervals represent abritrary units times. Here, lets us imagine they
   represent seconds."

  (-> imap/empty
      ;; Broken C minor chord.
      (imap/mark  0  8 :c)
      (imap/mark  3  8 :e-flat)
      (imap/mark  5  8 :g)
      ;; After a pause, G is repeated.
      (imap/mark 10 11 :g)))



(comment


  ;; What is happening at second 4?
  ;;
  ;; Cand E-Flat are being played.
  ;;
  (= (get music
          4)

     #{:c
       :e-flat})


  ;; Nothing was being played at second 9.
  ;;
  (nil? (get music
             9))


  ;; Expressed as a sequence, we see exactly how notes are played, when they start,
  ;; when they end, when some are played together.
  ;;
  ;; These are `segments` (ie. tuples of `[interval value-set]`).
  ;;
  (= (seq music)

     (list [[0 3]   #{:c}]
           [[3 5]   #{:c
                      :e-flat}]
           [[5 8]   #{:c
                      :e-flat
                      :g}]
           [[10 11] #{:g}]))


  ;; Using `subseq` from the standard library, we can directly query segments involved in
  ;; a given interval only.
  ;;
  ;; What happened during second 2 and second 4, included?
  ;; The returned segments are the most minimal answer covering that interval.
  ;;
  (= (subseq music
             >= 2
             <= 4)

     (list [[0 3] #{:c}]
           [[3 5] #{:c
                    :e-flat}]))



  ;; We can query segments even further:
  
  ;; Let us see exactly what notes are played at what intervals.
  ;;
  ;; G has two intervals. Indeed, it is played during the broken chord and
  ;; on its own a bit later, so:
  ;;
  (= (imap/by-value (seq music))
    
     {:c      #{[ 0  8]}
      :e-flat #{[ 3  8]}
      :g      #{[ 5  8]
                [10 11]}})


  ;; What notes are being played, in total?
  ;;
  (= (imap/union (seq music))
     
     #{:c
       :e-flat
       :g})



  ;; Modifying our C note and starting it later.
  ;;
  (def music-2
       (imap/erase music
                   0
                   4
                   :c))

  
  (nil? (get music-2
             1))


  (= (get music-2
          4)

     #{:c
       :e-flat})


  ;; Erasing is very flexible and permissive.
  ;;
  (= music
     (imap/erase music
                 0
                 56456
                 :whatever))
  )
