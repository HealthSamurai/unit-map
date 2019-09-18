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

(deftest parse-format-test
  (testing "parse"

    (matcho/match
     (ch/parse "2011-01-01")
     {:year 2011 :month 1 :day 1})

    (matcho/match
     (ch/parse "2011-01-01T12:00")
     {:year 2011 :month 1 :day 1 :hour 12 :min 0})

    (matcho/match
     (ch/parse "2011-01-01T12:00:00")
     {:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0})

    (matcho/match
     (ch/parse "2011-01-01T12:04:05.100")
     {:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100})

    (matcho/match
     (ch/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])
     {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}))

  (testing "format"
    (is (= "12/01/2010" (ch/format {:year 2010 :month 12 :day 1} [:month "/" :day "/" :year]))))

  (testing "roundtrip"
    (let [t {:year 2019, :month 9, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (matcho/match (ch/parse (ch/format t)) t))))

(deftest plus-test
  (matcho/match
   (ch/+ t {:ms 200})
   {:ms 700})

  (matcho/match
   (ch/+ t {:ms 600})
   {:ms 100
    :sec 31})

  (matcho/match
   (ch/+ t {:sec 20})
   {:sec 50})

  (matcho/match
   (ch/+ t {:sec 20})
   {:sec 50})

  (matcho/match
   (ch/+ t {:min 20})
   {:hour 12
    :min 50})

  (matcho/match
   (ch/+ t {:min 30})
   {:hour 13
    :min 0})

  (matcho/match
   (ch/+ {:year 2018 :month 12 :day 31} {:day 1})
   {:year 2019 :month 1 :day 1})

  (matcho/match
   (ch/+ {:year 2018 :month 1 :day 1} {:day 31})
   {:year 2018 :month 2 :day 1})

  (matcho/match
   (ch/+ {:year 2018 :month 12 :day 31} {:day 366})
   {:year 2020 :month 1 :day 1})

  (matcho/match
   (ch/+ {:year 2018 :month 2 :day 28} {:day 1})
   {:year 2018 :month 3 :day 1})

  (matcho/match
   (ch/+ {:year 2018 :month 3 :day 30} {:day 1})
   {:year 2018 :month 3 :day 31})

  (matcho/match
   (ch/+ {:year 2018 :month 3 :day 31} {:day 1})
   {:year 2018 :month 4 :day 1})


  ;; d(2010,3,14,7,0,0),
  (is (= 14 (ch/more-or-eq 2010 3 0 8)))
  ;; d(2010,11,7,6,0,0),
  (is (= 7 (ch/more-or-eq 2010 11 0 1)))

  ;; d(2017,3,12,7,0,0),
  (is (= 12 (ch/more-or-eq 2017 3 0 8)))

  ;; d(2017,11,5,6,0,0),
  (is (= 5 (ch/more-or-eq 2017 11 0 1)))

  ;; d(2018,3,11,7,0,0),
  (is (= 11 (ch/more-or-eq 2018 3 0 8)))

  ;; d(2018,11,4,6,0,0),
  (is (=  4 (ch/more-or-eq 2018 11 0 1)))
  ;; d(2019,3,10,7,0,0),
  (is (= 10 (ch/more-or-eq 2019 3 0 8)))
  ;; d(2019,11,3,6,0,0),
  (is (= 3 (ch/more-or-eq 2019 11 0 1))) 
  ;; d(2020,3,8,7,0,0),
  (is (= 8 (ch/more-or-eq 2020 3 0 8)))
  ;; d(2020,11,1,6,0,0),
  (is (= 1 (ch/more-or-eq 2020 11 0 1)))

  ;; (is (= 5 (ch/offset-for {:year 2010 :month 1 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 5 (ch/offset-for {:year 2010 :month 2 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 5 (ch/offset-for {:year 2010 :month 3 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 5 (ch/offset-for {:year 2010 :month 3 :day 14 :hour 0 :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 3 :day 14 :hour 2 :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 3 :day 20 :hour 0 :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 4 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 5 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 6 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 7 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 8 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 9 :day 1 :hour 0  :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 10 :day 1 :hour 0 :ch :ny})))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 11 :day 1 :hour 0 :ch :ny })))
  ;; (is (= 4 (ch/offset-for {:year 2010 :month 11 :day 7 :hour 0 :ch :ny })))
  ;; (is (= 5 (ch/offset-for {:year 2010 :month 11 :day 7 :hour 2 :ch :ny })))
  ;; (is (= 5 (ch/offset-for {:year 2010 :month 11 :day 20 :hour 0 :ch :ny})))
  ;; (is (= 5 (ch/offset-for {:year 2010 :month 12 :day 1 :hour 0 :ch :ny})))

  (matcho/match
   (ch/+ {:ms 100} {:ms 300})
   {:ms 400})

  (matcho/match
   (ch/+ {:ms 900} {:ms 300})
   {:ms 200 :sec 1})

  (matcho/match
   (ch/+ {:sec 40} {:sec 50})
   {:sec 30 :min 1})

  (matcho/match
   (ch/+ {:min 40} {:min 50})
   {:min 30 :hour 1})

  (matcho/match
   (ch/+ {:hour 13} {:hour 14})
   {:hour 3 :day 1})

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 1 :hour 23} {:hour 5})
   {:year 2011 :month 1 :day 2 :hour 4})

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 30} {:day 3})
   {:year 2011 :month 2 :day 2})

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 1} {:day 365})
   {:year 2012 :month 1 :day 1})

  (matcho/match
   (ch/+ {:year 2011 :month 12 :day 31 :hour 23} {:hour 5})
   {:year 2012 :month 1 :day 1 :hour 4})

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 1 :hour 0} {:hour -1})
   {:year 2010 :month 12 :day 31 :hour 23})

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 1 :hour 0} {:sec -1})
   {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59})

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 1 :hour 0} {:ms -1})
   {:year 2010 :month 12 :day 31 :hour 23 :min 59 :sec 59 :ms 999})

  (is (= [2011 4 10] (ch/days-and-months 2011 1 100)))
  (is (= [2011 2 1] (ch/days-and-months 2011 1 32)))

  (is (= [2012 1 1] (ch/days-and-months 2011 1 366)))
  (is (= [2013 1 1] (ch/days-and-months 2012 1 366)))
  (ch/is-leap? 2013)

  (is (= [2012 12 31] (ch/days-and-months 2013 1 0)))
  (is (= [2012 12 30] (ch/days-and-months 2013 1 -1)))

  (is (= [2013 1 1] (ch/days-and-months 2013 1 1)))
  (is (= [2013 1 31] (ch/days-and-months 2013 1 31)))
  (is (= [2013 2 1] (ch/days-and-months 2013 1 32)))


  (is (= [2012 1 1] (ch/days-and-months 2013 1 -364)))

  (matcho/match
   (ch/+ {:year 2011 :month 1 :day 1 :hour 23} {:hour -23 :min -30})
   {:year 2010 :month 12 :day 31 :hour 23 :min 30})













  )
