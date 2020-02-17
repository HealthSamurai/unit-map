(ns chrono.mask-test
  (:require [chrono.mask :as sut]
            [clojure.test :as t]))

(t/deftest mask-test
  (let [facts {"2300"     "23:00"
               "23:00"    "23:00"
               "230000"   "23:00:00"
               "23:00:00" "23:00:00"
               ""         ""
               "2"        "2"
               nil        ""
               "123456"   "12:34:56"
                                        ;"999999"   ""
               }
        chrono-fmt [:hour \: :min \: :sec]]
    (doseq [[inp res] facts]
      (t/is (= res (sut/mask chrono-fmt inp))))))
