(ns chrono.mask-test
  (:require [chrono.mask :as sut]
            [clojure.test :as t]
            [chrono.io :as io]))

(t/deftest mask-test
  (let [cases {[:hour \: :min \: :sec]
               {"2300"     "23:00:"
                "23:00"    "23:00:"
                "230000"   "23:00:00"
                "23:00:00" "23:00:00"
                ""         ""
                "2"        "2"
                "02:2"     "02:2"
                "02:2:"    "02:02:"
                "02:20"    "02:20:"
                "02:02:2"  "02:02:2"
                nil        ""
                "123456"   "12:34:56"
                "2:2"      "02:2"
                "2:2:"     "02:02:"
                "2:2:2"    "02:02:2"
                "399"      "03:09:09"
                "999999"   "09:09:09"}

               [:month \- :day]
               {"01-01" "01-01"
                "0101"  "01-01"
                "67"    "06-07"
                "!!!"   ""
                "0!"    "0"
                "!0"    "0"
                "11-!"  "11-"
                "11-!0" "11-0"
                "11-0!" "11-0"
                "1"     "1"
                "1!"    "1"
                "11"    "11-"
                "331"   "03-31"
                "12"    "12-"
                "0"     "0"
                "9"     "09-"
                ""      ""}}]
    (doseq [[fmt facts] cases
            [inp res]   facts]
      (t/testing (str "resolve: " fmt " " inp)
        (t/is (= res (sut/resolve inp fmt))))))

  (let [cases {[:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "+03:00"]
               {"2020-07-14T13:30"    "2020-07-14T13:30"
                "2020-07-14T13:30:00" "2020-07-14T13:30:00+03:00"}}]
    (doseq [[fmt facts] cases
            [inp res]   facts]
      (t/testing (str "resolve: " fmt " " inp)
        (t/is (= res (sut/clean-resolve inp fmt)))))))
