(ns chrono.core-test
  (:require [clojure.test :refer :all]
            [matcho.core :as matcho]
            [chrono.core :as ch]))

(def t
  {:year 2018
   :month 1
   :day 1
   :hour 12
   :min 30
   :sec 30 
   :ms 500})

(deftest plus-test
  (matcho/match
   (ch/plus t {:ms 200})
   {:ms 700})

  (matcho/match
   (ch/plus t {:ms 600})
   {:ms 100
    :sec 31})

  (matcho/match
   (ch/plus t {:sec 20})
   {:sec 50})

  (matcho/match
   (ch/plus t {:sec 20})
   {:sec 50})

  (matcho/match
   (ch/plus t {:min 20})
   {:hour 12
    :min 50})

  (matcho/match
   (ch/plus t {:min 30})
   {:hour 13
    :min 0})

  (matcho/match
   (ch/plus {:year 2018 :month 12 :day 31} {:day 1})
   {:year 2019 :month 1 :day 1})

  (matcho/match
   (ch/plus {:year 2018 :month 2 :day 28} {:day 1})
   {:year 2018 :month 3 :day 1})

  (matcho/match
   (ch/plus {:year 2018 :month 3 :day 30} {:day 1})
   {:year 2018 :month 3 :day 31})

  (matcho/match
   (ch/plus {:year 2018 :month 3 :day 31} {:day 1})
   {:year 2018 :month 4 :day 1})


  )
