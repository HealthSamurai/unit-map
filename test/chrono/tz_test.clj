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

  ;; (is (= 5 (tz/offset-for {:y 2010 :m 1 :d 1 :h 0  :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:y 2010 :m 2 :d 1 :h 0  :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:y 2010 :m 3 :d 1 :h 0  :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:y 2010 :m 3 :d 14 :h 0 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 3 :d 14 :h 2 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 3 :d 20 :h 0 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 4 :d 1 :h 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 5 :d 1 :h 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 6 :d 1 :h 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 7 :d 1 :h 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 8 :d 1 :h 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 9 :d 1 :h 0  :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 10 :d 1 :h 0 :tz :ny})))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 11 :d 1 :h 0 :tz :ny })))
  ;; (is (= 4 (tz/offset-for {:y 2010 :m 11 :d 7 :h 0 :tz :ny })))
  ;; (is (= 5 (tz/offset-for {:y 2010 :m 11 :d 7 :h 2 :tz :ny })))
  ;; (is (= 5 (tz/offset-for {:y 2010 :m 11 :d 20 :h 0 :tz :ny})))
  ;; (is (= 5 (tz/offset-for {:y 2010 :m 12 :d 1 :h 0 :tz :ny})))


  (is (= "12/1/2010" (tz/format {:y 2010 :m 12 :d 1} [:m "/" :d "/" :y])))
  

  (testing "parse"

    (matcho/match
     (tz/parse "2011-01-01")
     {:y 2011 :m 1 :d 1})

    (matcho/match
     (tz/parse "2011-01-01T12:00")
     {:y 2011 :m 1 :d 1 :h 12 :mi 0})

    (matcho/match
     (tz/parse "2011-01-01T12:00:00")
     {:y 2011 :m 1 :d 1 :h 12 :mi 0 :s 0})

    (matcho/match
     (tz/parse "2011-01-01T12:04:05.100")
     {:y 2011 :m 1 :d 1 :h 12 :mi 4 :s 5 :ms 100}))

  (matcho/match
   (tz/+ {:ms 100} {:ms 300})
   {:ms 400})

  (matcho/match
   (tz/+ {:ms 900} {:ms 300})
   {:ms 200 :s 1})

  (matcho/match
   (tz/+ {:s 40} {:s 50})
   {:s 30 :mi 1})

  (matcho/match
   (tz/+ {:mi 40} {:mi 50})
   {:mi 30 :h 1})

  (matcho/match
   (tz/+ {:h 13} {:h 14})
   {:h 3 :d 1})

  (matcho/match
   (tz/+ {:y 2011 :m 1 :d 1 :h 23} {:h 5})
   {:y 2011 :m 1 :d 2 :h 4})

  (matcho/match
   (tz/+ {:y 2011 :m 1 :d 30} {:d 3})
   {:y 2011 :m 2 :d 2})

  (matcho/match
   (tz/+ {:y 2011 :m 1 :d 1} {:d 365})
   {:y 2012 :m 1 :d 1})

  (matcho/match
   (tz/+ {:y 2011 :m 12 :d 31 :h 23} {:h 5})
   {:y 2012 :m 1 :d 1 :h 4})

  (matcho/match
   (tz/+ {:y 2011 :m 1 :d 1 :h 0} {:h -1})
   {:y 2010 :m 12 :d 31 :h 23})

  (matcho/match
   (tz/+ {:y 2011 :m 1 :d 1 :h 0} {:s -1})
   {:y 2010 :m 12 :d 31 :h 23 :mi 59 :s 59})

  (matcho/match
   (tz/+ {:y 2011 :m 1 :d 1 :h 0} {:ms -1})
   {:y 2010 :m 12 :d 31 :h 23 :mi 59 :s 59 :ms 999})

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
   (tz/+ {:y 2011 :m 1 :d 1 :h 23} {:h -23 :mi -30})
   {:y 2010 :m 12 :d 31 :h 23 :mi 30}))

(deftest test-timezones
  (matcho/match
   (tz/day-saving :ny 2017)
   {:in {:m 3 :d 12 :h 2 :mi 0}
    :out {:m 11 :d 5 :h 2}})

  (matcho/match
   (tz/day-saving-with-utc :ny 2017)
   {:in     {:m 3 :d 12 :h 2 :mi 0}
    :in-utc {:m 3 :d 12 :h 7}

    :out     {:m 11 :d 5 :h 2}
    :out-utc {:m 11 :d 5 :h 6}})

  (matcho/match
   (tz/day-saving-with-utc :ny 2018)
   {:in {:m 3 :d 11}
    :out {:m 11 :d 4}})

  (matcho/match
   (tz/day-saving-with-utc :ny 2019)
   {:in {:m 3 :d 10}
    :out {:m 11 :d 3}})
  

  (is (tz/before=? {:y 2018 :m 5 :d 2 :h 14 :tz :ny}
                   {:y 2018 :m 5 :d 2 :h 14 :tz :ny}))

  (is (tz/before=?
       {:y 2018 :m 5 :d 2 :h 14 :tz :ny}
       {:y 2018 :m 5 :d 2 :h 14 :s 1 :tz :ny}))

  (is (tz/after?
       {:y 2018 :m 5 :d 2 :h 14 :s 1 :tz :ny}
       {:y 2018 :m 5 :d 2 :h 14 :tz :ny}))

  (is (tz/before=? {:y 2018 :m 5 :d 2 :h 14 :tz :ny}
                   {:y 2018 :m 11}))

  (is (tz/after?
       {:y 2018 :m 11}
       {:y 2018 :m 5 :d 2 :h 14 :tz :ny}))

  (matcho/match
   (tz/to-utc {:y 2018 :m 5 :d 2 :h 14 :tz :ny})
   {:y 2018 :m 5 :d 2 :h 18})

  (matcho/match
   (tz/to-tz {:y 2018 :m 5 :d 2 :h 18} :ny)
   {:y 2018 :m 5 :d 2 :h 14 :tz :ny})


  (matcho/match
   (tz/to-utc {:y 2018 :m 2 :d 2 :h 14 :tz :ny})
   {:y 2018 :m 2 :d 2 :h 19})

  (matcho/match
   (tz/to-tz {:y 2018 :m 2 :d 2 :h 19} :ny)
   {:y 2018 :m 2 :d 2 :h 14 :tz :ny})

  )
