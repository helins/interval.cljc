(ns user

  "For daydreaming in the REPL." 

  (:require [criterium.core       :as criterium]
            [dvlopt.interval      :as interval]
            [dvlopt.interval-test :as interval-test]))


;;;;;;;;;;


(require '[nrepl.server])  (defonce server (nrepl.server/start-server :port 4000))



(comment


  (def tree
       (-> (interval/tree)
           (interval/assoc 0 15 :a)
           (interval/assoc 12 nil :b)
           (interval/assoc 20 25 :c)
           (interval/assoc 30 nil :d)
           (interval/assoc 35 40 :e)
           )
       )



  )
