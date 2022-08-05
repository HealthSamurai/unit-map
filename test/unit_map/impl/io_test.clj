(ns unit-map.impl.io-test
  (:require [unit-map.impl.io :as sut]
            [unit-map.impl.registrator :as registrator]
            [unit-map.impl.system :as system]
            [unit-map.impl.registry :as registry]
            [clojure.test :as t]
            [clojure.string :as str]))


(defn leap-year? [{:keys [year]}]
  (and (zero? (rem year 4))
       (or (pos? (rem year 100))
           (zero? (rem year 400)))))


(defn days-in-month [{:as date, :keys [month]}]
  (condp contains? month
    #{:jan :mar :may :jul :aug :oct :dec} 31
    #{:apr :jun :sep :nov}                30
    #{:feb}                               (if (leap-year? date) 29 28)
    ##Inf))

(def ns->sec     {:unit :ns,    :next-unit :sec,   :useq #unit-map/useq[0 1 .. 999999999]})
(def ms->sec     {:unit :ms,    :next-unit :sec,   :useq #unit-map/useq[0 1 .. 999]})
(def sec->min    {:unit :sec,   :next-unit :min,   :useq #unit-map/useq[0 1 .. 59]})
(def min->hour   {:unit :min,   :next-unit :hour,  :useq #unit-map/useq[0 1 .. 59]})
(def hour->day   {:unit :hour,  :next-unit :day,   :useq #unit-map/useq[0 1 .. 23]})
(def day->month  {:unit :day,   :next-unit :month, :useq #unit-map/useq[0 1 .. days-in-month]})
(def month->year {:unit :month, :next-unit :year,  :useq #unit-map/useq[:jan :feb  :mar :apr :may  :jun :jul :aug  :sep :oct :nov  :dec]})
(def years       {:unit :year,                     :useq #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf]})

(def date [:day :month :year])
(def datetime [:ms :sec :min :hour :day :month :year])

(def reg_ (registrator/new-registry))

(registrator/reg-useqs! reg_ [ns->sec ms->sec sec->min min->hour hour->day day->month month->year years])
(registrator/reg-systems! reg_ [date datetime])

(def base-formats
  {:ms {:unit  :ms
        :parse (fn [_reg s] (parse-long s))}

   :sec {:unit  :sec
         :parse (fn [_reg s] (parse-long s))}

   :min {:unit  :min
         :parse (fn [_reg s] (parse-long s))}

   :hour {:unit  :hour
          :parse (fn [_reg s] (parse-long s))}

   :day {:unit  :day
         :parse (fn [_reg s] (parse-long s))}

   :month {:unit   :month
           :parse  (fn [reg s] (system/useq-nth (registry/useq reg :month :year) {} (dec (parse-long s))))
           :format (fn [reg v] (inc (system/useq-index-of (registry/useq reg :month :year) {} v)))}

   :year {:unit  :year
          :parse (fn [_reg s] (parse-long s))}})

(doseq [[format-name format-params] base-formats]
  (sut/reg-format! reg_ format-name format-params))


#_(t/deftest name-test
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
    (t/is (= "November" (sut/format-el {:month :nov} nil ^:en[:month])))
    (t/is (= "Март" (sut/format-el {:month :mar} nil ^:ru[:month])))
    (t/is (= "Aug" (sut/format-el {:month :aug} nil ^:en[:month :short])))
    (t/is (= "09" (sut/format-el {:month :sep} nil [:month :short])))))


(t/deftest parse&format-test
  (def iso-fmt [[:year \0 4] "-" [:month \0 2] "-" [:day \0 2] "T" [:hour \0 2] ":" [:min \0 2] ":" [:sec \0 2] "." [:ms \0 3]])

  (t/testing "nil-safe"
    (t/is (= {} (sut/parse @reg_ nil nil)))
    #_(t/is (= "01" (sut/format @reg_ nil [:day])))
    (t/is (= "" (sut/format @reg_ nil nil))))


  (t/testing "parse"
    (t/testing "numeral representation of month"
      (t/is (= {:year 2011 :month :jan :day 1}
               (sut/parse @reg_ "2011-01-01" iso-fmt)))

      (t/is (= {:year 2011 :month :jan :day 1 :hour 12 :min 0}
               (sut/parse @reg_ "2011-01-01T12:00" iso-fmt)))

      (t/is (= {:year 2011 :month :jan :day 1 :hour 12 :min 0 :sec 0}
               (sut/parse @reg_ "2011-01-01T12:00:00" iso-fmt)))

      (t/is (= {:year 2011 :month :jan :day 1 :hour 12 :min 4 :sec 5 :ms 100}
               (sut/parse @reg_ "2011-01-01T12:04:05.100" iso-fmt)))

      (t/is (= {:day 16, :month :sep, :year 2019, :hour 23, :min 59, :sec 1}
               (sut/parse @reg_ "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec])))))

  (t/testing "format"
    (t/is (= "12/01/2010" (sut/format @reg_ {:year 2010 :month :dec :day 1} [:month "/" [:day \0 2] "/" :year]))))

  (t/testing "roundtrip"
    (let [t {:year 2019, :month :sep, :day 16, :hour 23, :min 0, :sec 38, :ms 911}]
      (t/is (= t (sut/parse @reg_ (sut/format @reg_ t iso-fmt) iso-fmt)))))

  (t/testing "format with specified width"
    (t/is (= "06.03.20"
             (sut/format @reg_ {:year 2020 :month :mar :day 6} [[:day \0 2] \. [:month \0 2] \. [:year \0 2]])))
    (t/is (= "06.03.2020"
             (sut/format @reg_ {:year 2020 :month :mar :day 6} [[:day \0 2] \. [:month \0 2] \. :year])))
    (t/is (= "2020.03.06"
             (sut/format @reg_ {:year 2020 :month :mar :day 6} [:year \. [:month \0 2] \. [:day \0 2]])))
    (t/is (= "20.03.06"
             (sut/format @reg_ {:year 2020 :month :mar :day 6} [[:year \0 2] \. [:month \0 2] \. [:day \0 2]])))
    (t/is (= "--1+ baz"
             (sut/format @reg_ {:foo 1, :bar "baz"} [[:foo 3 \-] \+ [:bar 4]]))))

  (t/testing "parse should return parsed value even if format not strictly cosistent"
    (t/is (= {:year 2011}
             (sut/parse @reg_ "2011-01-01" [:year "-" :month])))

    (t/is (= {:year 2011, :month :jan}
             (sut/parse @reg_ "2011-01-01" [:year "-" [:month \0 2]])))

    (t/is (= {:year 2011 :month :jan :day 1}
             (sut/parse @reg_ "2011-01-01" [:year "-" :month "-" :day "T" :hour]))))

  (t/testing "parsing invalid strings should return only parsed part"
    (t/is (= {:year 2020 :month :dec}
             (sut/parse @reg_ "2020-12-ab" [:year "-" :month "-" :day]))))

  (t/testing "strict-parse"
    (t/testing "strict parse should return value when format exact match"
      (t/is (= {:year 2011 :month :jan :day 1}
               (sut/parse @reg_ "2011-01-01" [:year \- :month \- :day] :strict true)))

      (t/is (= {:day 16, :month :sep, :year 2019, :hour 23, :min 59, :sec 1}
               (sut/parse @reg_ "16.09.2019 23:59:01" [:day \. :month \. :year \space :hour \: :min \: :sec] :strict true))))

    (t/testing "strict parse should return nil when format not strictly consistent"
      (t/is (= nil
               (sut/parse @reg_ "2011-01-" [:year "-" :month "-" :day] :strict true)))

      (t/is (=
              nil
              (sut/parse @reg_ "2011-01-01" [:year "-" :month "-" :day "T" :hour] :strict true))))

    (t/testing "parsing invalid strings should return nil"
      (t/is (=
              nil
              (sut/parse @reg_ "2011!23" [:year "-" :month] :strict true)))

      (t/is (=
              nil
              (sut/parse @reg_ "2011-12" [:year "-" :month "-" :day] :strict true)))))

  (t/testing "format-str"
    #_(t/testing "literal representation of month"
      (t/testing "default lang"
        (t/is (=
                {:day 16, :month :jan, :year 2019, :hour 23, :min 59, :sec 1}
                (sut/parse @reg_ "16 Jan 2019 23:59:01"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))

        (t/is (=
                {:day 31, :month :dec, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse @reg_ "31 December 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 31, :month :mar, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse @reg_ "31 march 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 28, :month :feb, :year 2023, :hour 13, :min 30, :sec 19}
                (sut/parse @reg_ "28 FEB 2023 13:30:19"
                           [:day \space :month \space :year \space :hour \: :min \: :sec])))
        (t/is (=
                {:day 28, :month :jun, :year 9999}
                (sut/parse @reg_ "jun. 28 9999" [:month \space :day \space :year]))))

      (t/testing "en"
        (t/is (=
                {:day 19, :month :sep, :year 2023}
                (sut/parse @reg_ "sep. 19 2023" ^:en[:month \space :day \space :year]))))

      (t/testing "ru"
        (t/is (=
                {:day 19 :month :jan}
                (sut/parse @reg_ "19 января" [:day \space ^:ru[:month]])))
        (t/is (=
                {:year 19 :month :feb}
                (sut/parse @reg_ "февраль 19" [^:ru[:month] \space :year])))
        (t/is (=
                {:month :oct :hour 9 :min 36}
                (sut/parse @reg_ "окт 9:36" [^:ru[:month] \space :hour \: :min]))))

      (t/testing "invalid month name"
        (t/is (= {:year 19}
                 (sut/parse @reg_ "19 month" [:year \space ^:ru[:month]])))
        (t/is (= {:year 19}
                 (sut/parse @reg_ "month 19" [^:ru[:month] \space :year]))))

      (t/testing "invalid locale name"
        (t/is (= {:year 19} (sut/parse @reg_ "январь 19" [^:en[:month] \space :year]))))

      (t/testing "month-name"
        (t/is (= "Октябрь 2009"
                 (sut/format @reg_ {:year 2009 :month :oct} [^:ru[:month] \space :year])))
        (t/is (= "Sep. 1"
                 (sut/format @reg_ {:month :sep :day 1} [^:en[:month :short] \. \space [:day 1]])))
        (t/is (= "1:month:entest"
                 (sut/format @reg_ {:month :jan}
                             [^:en[:month (fn [v {:keys [value lang pad-str]}] (str (get v value) value lang pad-str)) "test"]])))
        (t/is (= "2"
                 (sut/format @reg_ {:month :sep :day 1} [[:month 0 (fn [& args] (count args))]]))))

      (t/testing "strict parsing month names"
        (t/is (=
                {:year 2011 :month :jul} (sut/parse @reg_ "2011-JUL" [:year "-" :month] :strict true)))))

    (t/testing "parsing formatting without separators"
      (t/testing "fmt vec with width"
        (def fmt [[:year 2 \0] [:month 2 \0] [:day 2 \0] [:hour 2 \0] [:min 2 \0]])

        (t/is (= {:year 21 :month :mar :day 2 :hour 8 :min 5}
                 (sut/parse @reg_ "2103020805" fmt)))

        (t/is (= "2103020805" (sut/format @reg_ {:year 21 :month :mar :day 2 :hour 8 :min 5} fmt)))

        (t/testing "default padding"
          (def fmt [[:year 2] [:month 2] [:day 2] [:hour 2] [:min 2]])

          (t/is (= {:year 21 :month :mar :day 2 :hour 8 :min 5}
                   (sut/parse @reg_ "21 3 2 8 5" fmt)))

          (t/is (= "21 3 2 8 5" (sut/format @reg_ {:year 21 :month :mar :day 2 :hour 8 :min 5} fmt)))))))


  (t/testing "map fmt el"
    (def s "200101")
    (def fmt [{:value :year,  :element :year,  :width 2, :pad "0"}
              {:value :month, :element :month, :width 2, :pad 0}
              {:value :day,   :element :day,   :width 2, :pad \0}])

    (t/is (= {:year 20, :month :jan, :day 1}
             (sut/parse @reg_ s fmt)))
    (t/is (= s
             (sut/format @reg_ {:year 2020, :month :jan, :day 1} fmt)))))


(t/deftest parse-format-cfg
  ;; 2015
  ;; 2015-04
  ;; 2015-04-25
  ;; 04/25/2015
  ;; 25 Апр 2015
  ;; 25 07 (апр.) 2015
  ;; Fri Apr 25 2015 12:48:59 GMT+0200 (Eastern European Standard Time)
  ;; 2015-06-30T23:59:60Z
  ;; Roman year

  (def user-formats
    {:DD {:element :day
          :width   2
          :pad     "0"}

     :MM {:element :month
          :width   2
          :pad     "0"}

     :YY {:element :year
          :parse   (fn [_reg s] (str "20" s))
          :width   2
          :pad     "0"}

     :YYYY {:element :year
            :width   4
            :pad     "0"}})

  (doseq [[format-name format-params] user-formats]
    (sut/reg-format! reg_ format-name format-params))

  (t/testing "04/2005; 04/05"
    (def d {:year 2005, :month :apr})
    (def s "04/2005")
    (def f [:MM "/" :YYYY])

    (t/is (= "2005" (sut/format-el @reg_ :YYYY d)))

    #_(t/is (= {:year 2005} (sut/parse-unit @reg_ :YYYY "2005")))

    (t/is (= "05" (sut/format-el @reg_ :YY d)))

    #_(t/is (= {:year 2005} (sut/parse-unit @reg_ :YY "05")))

    (t/is (= "04" (sut/format-el @reg_ :MM d)))

    #_(t/is (= {:month :apr} (sut/parse-unit @reg_ :MM "04")))

    (t/is (= s (sut/format @reg_ d f)))

    (t/is (= d (sut/parse @reg_ s f))))

  (t/testing "Saturday, Apr 25, 2005"
    (defn day-of-week
      "y > 1752"
      [year month day]
      (let [t {:jan 0, :feb 3
               :mar 2, :apr 5, :may 0
               :jun 3, :jul 5, :aug 1
               :sep 4, :oct 6, :nov 2
               :dec 4}
            y (if (contains? #{:jan :feb} month)
                (dec year)
                year)]
        (nth [:sun :mon :tue :wed :thu :fri :sat]
             (rem (+ y
                     (int (/ y 4))
                     (- (int (/ y 100)))
                     (int (/ y 400))
                     (get t month)
                     day)
                  7))))

    (def weekday-names
      {:mon {:full "Monday"    :short "Mon"}
       :tue {:full "Tuesday"   :short "Tue"}
       :wed {:full "Wednesday" :short "Tue"}
       :thu {:full "Thursday"  :short "Thu"}
       :fri {:full "Friday"    :short "Fri"}
       :sat {:full "Saturday"  :short "Sat"}
       :sun {:full "Sunday"    :short "Sun"}})

    (sut/reg-format! reg_
                 :month/weekday
                 {:units  #{:year :month :day}
                  :parse  (constantly nil)
                  :format (fn [_reg {:keys [year month day]}]
                            (get-in weekday-names [(day-of-week year month day) :full]))})

    (sut/reg-format! reg_
                 :month/weekday-short
                 {:units  #{:year :month :day}
                  :parse  (constantly nil)
                  :format (fn [_reg {:keys [year month day]}]
                            (get-in weekday-names [(day-of-week year month day) :short]))})

    (sut/reg-format! reg_
                 :month/short
                 {:unit   :month
                  :width  3
                  :pad    " "
                  :parse  (fn [_reg s] (keyword (str/lower-case s)))
                  :format (fn [_reg v] (str/capitalize (name v)))})

    (def d {:year 2005, :month :apr, :day 5})
    (def s "Tuesday, Apr 5, 2005")
    (def f [:month/weekday ", " :month/short " " :day ", " :year])

    (t/is (= "Apr" (sut/format-el @reg_ :month/short d)))

    #_(t/is (= {:month :apr} (sut/parse-unit @reg_ :month/short "Apr")))

    (t/is (= "5" (sut/format-el @reg_ :day d)))

    #_(t/is (= {:day 5} (sut/parse-unit @reg_ :day "5")))

    (t/is (= "Tuesday" (sut/format-el @reg_ :month/weekday d)))

    #_(t/is (= nil (sut/parse-unit @reg_ :month/weekday "Tuesday")))

    (t/is (= "Tue" (sut/format-el @reg_ :month/weekday-short d)))

    #_(t/is (= nil (sut/parse-unit @reg_ :month/weekday-short "Sat")))

    (t/is (= s (sut/format @reg_ d f)))

    (t/is (= d (sut/parse @reg_ s f))))

  (t/testing ":elements; m2y2 0f6m0t1 -> {:year 2022, :month :jun, :day 1}"
    (sut/reg-format! reg_
                 :YYMMDD
                 {:elements #{:YY :MM :DD}
                  :width 6
                  :parse (fn [_reg s]
                           {:YY (subs s 0 2)
                            :MM (subs s 2 4)
                            :DD (subs s 4 6)})
                  :format (fn [_reg {:as arg :keys [YY MM DD]}]
                            (str YY MM DD))})

    (sut/reg-format! reg_
                 ::my-format
                 {:element :YYMMDD
                  :width 12
                  :parse  (fn [_reg s]
                            (str/join (take-nth 2 (rest s))))
                  :format (fn [_reg YYMMDD]
                            (str/join (interleave "my fmt" YYMMDD)))})

    (def d {:year 2022, :month :jun, :day 1})
    (def s "m2y2 0f6m0t1")
    (def f [::my-format])

    (t/is (= "m2y2 0f6m0t1" (sut/format-el @reg_ ::my-format d)))

    #_(t/is (= d (sut/parse-unit @reg_ ::my-format "m2y2 0f6m0t1")))

    (t/is (= s (sut/format @reg_ d f)))

    (t/is (= d (sut/parse @reg_ s f))))

  (t/testing ":units"
    (sut/reg-format! reg_
                 ::read-string
                 {:units #{:day :month :year}
                  :parse (fn [_reg s] (read-string s))})

    (def d {:year 2022, :month :jun, :day 1})
    (def f [::read-string])

    (t/is (= d (read-string (sut/format-el @reg_ ::read-string d))))

    #_(t/is (= d (sut/parse-unit @reg_ ::read-string (str d))))

    (t/is (= d (read-string (sut/format @reg_ d f))))

    (t/is (= d (sut/parse @reg_ (str d) f))))


  (t/testing "roman numeric YYYY•MM•DD"
    (defn int->roman [i]
      (clojure.pprint/cl-format nil "~@R" i))

    (defn roman->int [r]
      (->> (reverse (str/upper-case r))
           (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1})
           (partition-by identity)
           (map (partial apply +))
           (reduce #(if (< %1 %2) (+ %1 %2) (- %1 %2)))))

    (sut/reg-format! reg_
                 :roman/year
                 {:unit   :year
                  :parse  (fn [_reg s] (roman->int s))
                  :format (fn [_reg i] (int->roman i))})

    (sut/reg-format! reg_
                 :roman/month
                 {:element :month
                  :parse  (fn [_reg s] (str (roman->int s)))
                  :format (fn [_reg s] (int->roman (parse-long s)))})

    (sut/reg-format! reg_
                 :roman/day
                 {:unit   :day
                  :parse  (fn [_reg s] (roman->int s))
                  :format (fn [_reg i] (int->roman i))})

    (def d {:year 2022, :month :jun, :day 1})
    (def s "MMXXII•VI•I")
    (def f [:roman/year "•" :roman/month "•" :roman/day])

    (t/is (= "MMXXII" (sut/format-el @reg_ :roman/year d)))

    #_(t/is (= {:year 2022} (sut/parse-unit @reg_ :roman/year "MMXXII")))

    (t/is (= "VI" (sut/format-el @reg_ :roman/month d)))

    #_(t/is (= {:month :jun} (sut/parse-unit @reg_ :roman/month "VI")))

    (t/is (= "I" (sut/format-el @reg_ :roman/day d)))

    #_(t/is (= {:day 1} (sut/parse-unit @reg_ :roman/day "I")))

    (t/is (= s (sut/format @reg_ d f)))

    (t/is (= d (sut/parse @reg_ s f)))))
