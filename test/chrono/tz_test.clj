(ns chrono.tz-test
  (:require [chrono.tz :as tz]
            [clojure.test :refer :all]
            [matcho.core :as matcho])
  (:import [java.util Date]))


(deftest tz-test

  ;; d(2010,3,14,7,0,0),
  (is (= 14 (tz/more-or-eq 2010 3 0 8)))
  ;; d(2010,11,7,6,0,0),
  (is (= 7 (tz/more-or-eq 2010 11 0 1)))

  ;; d(2017,3,12,7,0,0),
  (is (= 12 (tz/more-or-eq 2017 3 0 8)))

  ;; d(2017,11,5,6,0,0),
  (is (= 5 (tz/more-or-eq 2017 11 0 1)))

  ;; d(2018,3,11,7,0,0),
  (is (= 11 (tz/more-or-eq 2018 3 0 8)))

  ;; d(2018,11,4,6,0,0),
  (is (=  4 (tz/more-or-eq 2018 11 0 1)))
  ;; d(2019,3,10,7,0,0),
  (is (= 10 (tz/more-or-eq 2019 3 0 8)))
  ;; d(2019,11,3,6,0,0),
  (is (= 3 (tz/more-or-eq 2019 11 0 1))) 
  ;; d(2020,3,8,7,0,0),
  (is (= 8 (tz/more-or-eq 2020 3 0 8)))
  ;; d(2020,11,1,6,0,0),
  (is (= 1 (tz/more-or-eq 2020 11 0 1)))

  ;; (is (= 5 (tz/offset-for {:year 2010 :month 1 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:year 2010 :month 2 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:year 2010 :month 3 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:year 2010 :month 3 :day 14 :hour 0 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 3 :day 14 :hour 2 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 3 :day 20 :hour 0 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 4 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 5 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 6 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 7 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 8 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 9 :day 1 :hour 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 10 :day 1 :hour 0 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 11 :day 1 :hour 0 :tz :ny })))
  ;; (is (= 4 (tz/offset-for {:year 2010 :month 11 :day 7 :hour 0 :tz :ny })))
  ;; (is (= 5 (tz/offset-for {:year 2010 :month 11 :day 7 :hour 2 :tz :ny })))
  ;; (is (= 5 (tz/offset-for {:year 2010 :month 11 :day 20 :hour 0 :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:year 2010 :month 12 :day 1 :hour 0 :tz :ny})))


  (is (= "12/1/2010" (tz/format {:year 2010 :month 12 :day 1} [:month "/" :day "/" :year])))
  

  (testing "parse"

    (matcho/match
     (tz/parse "2011-01-01")
     {:year 2011 :month 1 :day 1})

    (matcho/match
     (tz/parse "2011-01-01T12:00")
     {:year 2011 :month 1 :day 1 :hour 12 :min 0})

    (matcho/match
     (tz/parse "2011-01-01T12:00:00")
     {:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0})

    (matcho/match
     (tz/parse "2011-01-01T12:04:05.100")
     {:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100}))

  (matcho/match
   (tz/+ {:ms 100} {:ms 300})
   {:ms 400})

  (matcho/match
   (tz/+ {:ms 900} {:ms 300})
   {:ms 200 :sec 1})

  (matcho/match
   (tz/+ {:sec 40} {:sec 50})
   {:sec 30 :min 1})

  (matcho/match
   (tz/+ {:min 40} {:min 50})
   {:min 30 :hour 1})

  (matcho/match
   (tz/+ {:hour 13} {:hour 14})
   {:hour 3 :day 1})

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 1 :hour 23} {:hour 5})
   {:year 2011 :month 1 :day 2 :hour 4})

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 30} {:day 3})
   {:year 2011 :month 2 :day 2})

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 1} {:day 365})
   {:year 2012 :month 1 :day 1})

  (matcho/match
   (tz/+ {:year 2011 :month 12 :day 31 :hour 23} {:hour 5})
   {:year 2012 :month 1 :day 1 :hour 4})

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 1 :hour 0} {:hour -1})
   {:year 2010 :month 12 :day 31 :hour 23})

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 1 :hour 0} {:sec -1})
   {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59})

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 1 :hour 0} {:ms -1})
   {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999})

  (is (= [2011 4 10] (tz/days-and-months 2011 1 100)))
  (is (= [2011 2 1] (tz/days-and-months 2011 1 32)))

  (is (= [2012 1 1] (tz/days-and-months 2011 1 366)))
  (is (= [2013 1 1] (tz/days-and-months 2012 1 366)))

  (is (= [2012 12 31] (tz/days-and-months 2013 1 0)))
  (is (= [2012 12 30] (tz/days-and-months 2013 1 -1)))
  

  (is (= [2013 1 1] (tz/days-and-months 2013 1 1)))
  (is (= [2013 1 31] (tz/days-and-months 2013 1 31)))
  (is (= [2013 2 1] (tz/days-and-months 2013 1 32)))


  (is (= [2012 1 1] (tz/days-and-months 2013 1 -364)))

  (matcho/match
   (tz/+ {:year 2011 :month 1 :day 1 :hour 23} {:hour -23 :min -30})
   {:year 2010 :month 12 :day 31 :hour 23 :min 30}))

(deftest test-timezones
  (matcho/match
   (tz/day-saving :ny 2017)
   {:in {:month 3 :day 12 :hour 2 :min 0}
    :out {:month 11 :day 5 :hour 2}})

  (matcho/match
   (tz/day-saving-with-utc :ny 2017)
   {:in     {:month 3 :day 12 :hour 2 :min 0}
    :in-utc {:month 3 :day 12 :hour 7}

    :out     {:month 11 :day 5 :hour 2}
    :out-utc {:month 11 :day 5 :hour 6}})

  (matcho/match
   (tz/day-saving-with-utc :ny 2018)
   {:in {:month 3 :day 11}
    :out {:month 11 :day 4}})

  (matcho/match
   (tz/day-saving-with-utc :ny 2019)
   {:in {:month 3 :day 10}
    :out {:month 11 :day 3}})
  

  (is (tz/before=? {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                   {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

  (is (tz/before=?
       {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
       {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}))

  (is (tz/after?
       {:year 2018 :month 5 :day 2 :hour 14 :sec 1 :tz :ny}
       {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

  (is (tz/before=? {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}
                   {:year 2018 :month 11}))

  (is (tz/after?
       {:year 2018 :month 11}
       {:year 2018 :month 5 :day 2 :hour 14 :tz :ny}))

  (matcho/match
   (tz/to-utc {:year 2018 :month 5 :day 2 :hour 14 :tz :ny})
   {:year 2018 :month 5 :day 2 :hour 18})

  (matcho/match
   (tz/to-tz {:year 2018 :month 5 :day 2 :hour 18} :ny)
   {:year 2018 :month 5 :day 2 :hour 14 :tz :ny})


  (matcho/match
   (tz/to-utc {:year 2018 :month 2 :day 2 :hour 14 :tz :ny})
   {:year 2018 :month 2 :day 2 :hour 19})

  (matcho/match
   (tz/to-tz {:year 2018 :month 2 :day 2 :hour 19} :ny)
   {:year 2018 :month 2 :day 2 :hour 14 :tz :ny})

  )
