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
                "2"        "02:"
                nil        ""
                "123456"   "12:34:56"
                "2:2"      "02:02:"
                "2:2:"     "02:02:"
                "2:2:2"    "02:02:02"
                "399"      "03:09:09"
                "999999"   "09:09:09"}

               [:month \- :day]
               {"01-01"  "01-01"
                "0101"   "01-01"
                "11"     "01-01"
                "331"    "03-03"
                "12"     "12-"
                "0"      "0"
                ""       ""
                "1"      "1"}}]
    (doseq [[fmt facts] cases
            [inp res] facts]
      (t/is (= res (sut/resolve inp fmt)))))

  )
