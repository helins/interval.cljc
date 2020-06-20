(ns user

  "For daydreaming in the REPL." 

  (:require [criterium.core  :as criterium]
            [dvlopt.interval :as interval]))


;;;;;;;;;;


(require '[nrepl.server])  (defonce server (nrepl.server/start-server :port 4000))



(comment


  (def tree
       (-> (interval/tree)
           (interval/assoc 0 15 :a)
           (interval/assoc 10 12 :b)
           (interval/assoc 20 25 :c)
           ))


  )
