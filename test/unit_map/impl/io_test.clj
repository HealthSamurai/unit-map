(ns unit-map.impl.io-test
  (:require [unit-map.impl.io :as sut]
            [clojure.test :as t]))


(t/deftest name-test
  (t/testing "testing months parsing"
    (let [cases {"jan" 1
                 "FEB" 2
                 "March" 3
                 "april" 4
                 "MaY." 5
                 "JUNE" 6}
          test-fn (fn [[inp res]]
                    (t/testing (str "parsing: " inp)
                      (t/is (= (sut/parse-name inp :month nil) res))))]
      (doall
        (map test-fn cases))))

  (t/testing "month names formatting"
    (t/is (= "November" (sut/format-el {:month 11} nil ^:en[:month])))
    (t/is (= "Март" (sut/format-el {:month 3} nil ^:ru[:month])))
    (t/is (= "Aug" (sut/format-el {:month 8} nil ^:en[:month :short])))
    (t/is (= "09" (sut/format-el {:month 9} nil [:month :short])))))


(t/deftest parse&format-test
  (def iso-fmt [:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "." :ms])

  (t/testing "nil-safe"
    (t/is (= {} (sut/parse nil nil)))
    #_(t/is (= "01" (sut/format nil [:day])))
    (t/is (= "" (sut/format nil nil))))


  (t/testing "parse"
    (t/testing "numeral representation of month"
      (t/is (= {:year 2011 :month 1 :day 1}
               (sut/parse "2011-01-01" iso-fmt)))

      (t/is (= {:year 2011 :month 1 :day 1 :hour 12 :min 0}
               (sut/parse "2011-01-01T12:00" iso-fmt)))

      (t/is (= {:year 2011 :month 1 :day 1 :hour 12 :min 0 :sec 0}
               (sut/parse "2011-01-01T12:00:00" iso-fmt)))

      (t/is (= {:year 2011 :month 1 :day 1 :hour 12 :min 4 :sec 5 :ms 100}
               (sut/parse "2011-01-01T12:04:05.100" iso-fmt)))

      (t/is (= {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}
               (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])))))

  (t/testing "format"
    (t/is (= "12/01/2010" (sut/format {:year 2010 :month 12 :day 1} [:month "/" :day "/" :year]))))

  (t/testing "roundtrip"
    (let [t {:year 2019, :month 9, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (t/is (= t
               (-> t
                   (sut/format iso-fmt)
                   (sut/parse iso-fmt))))))

  (t/testing "format with specified width"
    (t/is (= "06.03.20"
             (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. [:year 2]])))
    (t/is (= "06.03.2020"
             (sut/format {:year 2020 :month 3 :day 6} [:day \. :month \. :year])))
    (t/is (= "2020.03.06"
             (sut/format {:year 2020 :month 3 :day 6} [:year \. :month \. :day])))
    (t/is (= "20.03.06"
             (sut/format {:year 2020 :month 3 :day 6} [[:year 2] \. :month \. :day])))
    (t/is (= "--1+ baz"
             (sut/format {:foo 1, :bar "baz"} [[:foo 3 \-] \+ [:bar 4]]))))

  (t/testing "parse should return parsed value even if format not strictly cosistent"
    (t/is (= {:year 2011}
             (sut/parse "2011-01-01" [:year "-" :month])))

    (t/is (= {:year 2011 :month 1 :day 1}
             (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour]))))

  (t/testing "parsing invalid strings should return only parsed part"
    (t/is (= {:year 2020 :month 12}
             (sut/parse "2020-12-ab" [:year "-" :month "-" :day]))))

  (t/testing "strict-parse"
    (t/testing "strict parse should return value when format exact match"
      (t/is (= {:year 2011 :month 1 :day 1}
               (sut/parse "2011-01-01" [:year \- :month \- :day] :strict true)))

      (t/is (= {:day 16, :month 9, :year 2019, :hour 23, :min 59, :sec 1}
               (sut/parse "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec] :strict true))))

    (t/testing "strict parse should return nil when format not strictly consistent"
      (t/is (= nil
               (sut/parse "2011-01-" [:year "-" :month "-" :day] :strict true)))

      (t/is (=
              nil
              (sut/parse "2011-01-01" [:year "-" :month "-" :day "T" :hour] :strict true))))

    (t/testing "parsing invalid strings should return nil"
      (t/is (=
              nil
              (sut/parse "2011!23" [:year "-" :month] :strict true)))

      (t/is (=
              nil
              (sut/parse "2011-12" [:year "-" :month "-" :day] :strict true)))))

  (t/testing "format-str"
    (t/testing "literal representation of month"
      (t/testing "default lang"
        (t/is (=
                {:day 16, :month 1, :year 2019, :hour 23, :min 59, :sec 1}
                (sut/parse "16 Jan 2019 23:59:01"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))

        (t/is (=
                {:day 31, :month 12, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse "31 December 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 31, :month 3, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse "31 march 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 28, :month 2, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse "28 FEB 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 28, :month 6, :year 9999}
                (sut/parse "jun. 28 9999" [:month \space :day \space :year]))))

      (t/testing "en"
        (t/is (=
                {:day 19, :month 9, :year 2023}
                (sut/parse "sep. 19 2023" ^:en[:month \space :day \space :year]))))

      (t/testing "ru"
        (t/is (=
                {:day 19 :month 1}
                (sut/parse "19 января" [:day \space ^:ru[:month]])))
        (t/is (=
                {:year 19 :month 2}
                (sut/parse "февраль 19" [^:ru[:month] \space :year])))
        (t/is (=
                {:month 10 :hour 9 :min 36}
                (sut/parse "окт 9:36" [^:ru[:month] \space :hour \: :min]))))

      (t/testing "invalid month name"
        (t/is (= {:year 19}
                 (sut/parse "19 month" [:year \space ^:ru[:month]])))
        (t/is (= {:year 19}
                 (sut/parse "month 19" [^:ru[:month] \space :year]))))

      (t/testing "invalid locale name"
        (t/is (= {:year 19} (sut/parse "январь 19" [^:en[:month] \space :year])))))

    (t/testing "month-name"
      (t/is (= "Октябрь 2009"
               (sut/format {:year 2009 :month 10} [^:ru[:month] \space :year])))
      (t/is (= "Sep. 1"
               (sut/format {:month 9 :day 1} [^:en[:month :short] \. \space [:day 1]])))
      (t/is (= "1:month:entest"
               (sut/format {:month 1}
                           [^:en[:month (fn [v {:keys [value lang pad-str]}] (str (get v value) value lang pad-str)) "test"]])))
      (t/is (= "2"
               (sut/format {:month 9 :day 1} [[:month 0 (fn [& args] (count args))]]))))

    (t/testing "strict parsing month names"
      (t/is (=
              {:year 2011 :month 7} (sut/parse "2011-JUL" [:year "-" :month] :strict true))))

    (t/testing "parsing formatting without separators"
      (t/testing "fmt vec with regex"
        (let [fmt [[:year #"\d\d" 2] [:month #"\d\d"] [:day #"\d\d"] [:hour #"\d\d"] [:min #"\d\d"]]]
          (t/is (= {:year 21 :month 3 :day 2 :hour 8 :min 5}
                   (sut/parse "2103020805" fmt)))

          (t/is (= "2103020805" (sut/format {:year 21 :month 3 :day 2 :hour 8 :min 5} fmt)))))

      (t/testing "fmt vec with width"
        (let [fmt [[:year 2] [:month 2] [:day 2] [:hour 2] [:min 2]]]
          (t/is (= {:year 21 :month 3 :day 2 :hour 8 :min 5}
                   (sut/parse "2103020805" fmt)))

          (t/is (= "2103020805" (sut/format {:year 21 :month 3 :day 2 :hour 8 :min 5} fmt))))))))
