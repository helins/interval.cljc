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
           (interval/mark 0 15 :a)
           (interval/mark 12 nil :b)
           (interval/mark 20 25 :c)
           (interval/mark 30 nil :d)
           (interval/mark 35 40 :e)
           )
       )



  )
