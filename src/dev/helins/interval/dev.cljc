;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.interval.dev

  "For daydreaming at the REPL."

  (:require [helins.interval.example.music]
            [helins.interval.map            :as imap]
            [helins.interval.set            :as iset]
            [helins.interval.util           :as interval.util]))


;;;;;;;;;;


(def im
     (-> imap/empty
         (imap/mark 0   15  :a)
         (imap/mark 12  nil :b)
         (imap/mark 20  25  :c)
         (imap/mark 30  nil :d)
         (imap/mark 35  40  :e)
         (imap/mark nil 0   :f)))



(comment



  )
