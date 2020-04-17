(ns chrono.io-test
  (:require [clojure.test :refer :all]
            [matcho.core :as matcho]
            [chrono.io :as sut]
            [chrono.locale-ru]))

(deftest parse-format-test
  (testing "parse"
    (testing "numeral representation of month"
      (matcho/match
       (sut/parse "2011-01-01")
       {:year 2011 :month 1 :day 1})

      (matcho/match
       (sut/parse "2011-01-01T12:00")
       {:year 2011 :month 1 :day 1 :hour 12 :min 0})

      (matcho/match
       (sut/parse "2011-01-01T12:00:00")
       {:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0})

      (matcho/match
       (sut/parse "2011-01-01T12:04:05.100")
       {:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100})

      (matcho/match
       (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])
       {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}))
    (testing "literal representation of month"
      (testing "default lang"
        (matcho/match
         (sut/parse "16 Jan 2019 23:59:01"
                    [:day \space :month \space :year \space :hour \: :min \: :sec])
         {:day 16, :month 1, :year 2019, :hour 23, :min 59, :sec 1})

        (matcho/match
         (sut/parse "31 December 2023 13:30:19"
                    [:day \space :month \space :year \space :hour \: :min \: :sec])
         {:day 31, :month 12, :year 2023, :hour 13, :min 30, :sec 19})
        (matcho/match
         (sut/parse "31 march 2023 13:30:19"
                    [:day \space :month \space :year \space :hour \: :min \: :sec])
         {:day 31, :month 3, :year 2023, :hour 13, :min 30, :sec 19})
        (matcho/match
         (sut/parse "28 FEB 2023 13:30:19"
                    [:day \space :month \space :year \space :hour \: :min \: :sec])
         {:day 28, :month 2, :year 2023, :hour 13, :min 30, :sec 19})
        (matcho/match
         (sut/parse "jun. 28 9999" [:month \space :day \space :year])
         {:day 28, :month 6, :year 9999}))
      (testing "en"
        (matcho/match
         (sut/parse "sep. 19 2023" ^:en[:month \space :day \space :year])
         {:day 19, :month 9, :year 2023}))
      (testing "ru"
        (matcho/match
         (sut/parse "19 января" ^:ru[:day \space :month])
         {:day 19 :month 1})
        (matcho/match
         (sut/parse "февраль 19" ^:ru[:month \space :year])
         {:year 19 :month 2})
        (matcho/match
         (sut/parse "окт 9:36" ^:ru[:month \space :hour \: :min])
         {:month 10 :hour 9 :min 36}))
      (testing "invalid month name"
        (is (thrown? Exception
                     (sut/parse "явнарь 19" ^:ru[:month \space :year]))))
      (testing "invalid locale name"
        (is (thrown? Exception
                     (sut/parse "январь 19" ^:en[:month \space :year]))))))

  (testing "format"
    (is (= "12/01/2010" (sut/format {:year 2010 :month 12 :day 1} [:month "/" :day "/" :year]))))

  (testing "roundtrip"
    (let [t {:year 2019, :month 9, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (matcho/match (sut/parse (sut/format t)) t)))

  (testing "format with specified width"
    (is
      (=
        "06.03.20"
        (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. [:year 2]])))
    (is
      (=
        "06.03.2020"
        (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. :year])))
    (is
      (=
        "2020.03.06"
        (sut/format {:year 2020 :month 3 :day 6} [:year \. :month \. :day])))
    (is
      (=
        "20.03.06"
        (sut/format {:year 2020 :month 3 :day 6} [[:year 2] \. :month \. :day]))))
  (testing "parse should return parsed value even if format not strictly cosistent"
    (matcho/match
     (sut/parse "2011-01-01" [:year "-" :month]) {:year 2011 :month 1})

    (matcho/match
     (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour]) {:year 2011 :month 1}))

  (testing "parsing invalid strings should return nil"
    (matcho/match
     (sut/parse "2011-12" [:month "-" :year]) nil)))

(deftest strict-parse-test
  (testing "strict parse should return value when format exact match"
    (matcho/match
     (sut/strict-parse "2011-01-01" [:year \- :month \- :day])
     {:year 2011 :month 1 :day 1})

    (matcho/match
     (sut/strict-parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])
     {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}))

  (testing "strict parse should return nil when format not strictly consistent"
    (matcho/match
     (sut/strict-parse "2011-01-01" [:year "-" :month])
     nil)

    (matcho/match
     (sut/strict-parse "2011-01-01" [:year "-" :month "-" :day "T" :hour])
     nil))

  (testing "parsing invalid strings should return nil"
        (matcho/match
     (sut/strict-parse "2011-23" [:year "-" :month]) nil)

    (matcho/match
     (sut/strict-parse "2011-12" [:month "-" :year]) nil))

  (testing "strict parsing month names"
    (matcho/match
     (sut/strict-parse "2011-JUL" [:year "-" :month]) {:year 2011 :month 7})))
