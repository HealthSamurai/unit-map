(ns chrono.mask-test
  (:require [chrono.mask :as sut]
            [clojure.test :as t]
            [chrono.io :as io]))

(t/deftest mask-test
  (let [facts {"2300"     "23:00:"
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
               #_#_"999999"   ""}
        chrono-fmt [:hour \: :min \: :sec]]
    (doseq [[inp res] facts]
      (t/is (= res (sut/resolve inp chrono-fmt))))))
