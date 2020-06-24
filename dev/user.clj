(ns user

  "For daydreaming in the REPL." 

  (:require [criterium.core            :as criterium]
            [dvlopt.interval.map       :as interval.map]
            [dvlopt.interval.map-test  :as interval.map-test]
            [dvlopt.interval.util      :as interval.util]
            [dvlopt.interval.util-test :as interval.util-test]))


;;;;;;;;;;


(require '[nrepl.server])  (defonce server (nrepl.server/start-server :port 4000))


(def tree
     (-> (interval.map/tree)
         (interval.map/mark 0   15  :a)
         (interval.map/mark 12  nil :b)
         (interval.map/mark 20  25  :c)
         (interval.map/mark 30  nil :d)
         (interval.map/mark 35  40  :e)
         (interval.map/mark nil 0   :f)))



(comment


  )
