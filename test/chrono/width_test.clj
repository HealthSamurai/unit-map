(ns chrono.width-test
  (:require [clojure.test :refer :all]
            [chrono.io :as cio]))

(deftest format-with-specified-width
  (is
    (=
      "06.03.20"
      (cio/format {:year 2020 :month 3 :day 6} [:day \. :month \. [:year 2]])))
  (is
    (=
      "06.03.2020"
      (cio/format {:year 2020 :month 3 :day 6} [:day \. :month \. :year])))
  (is
    (=
      "2020.03.06"
      (cio/format {:year 2020 :month 3 :day 6} [:year \. :month \. :day])))
  (is
    (=
      "20.03.06"
      (cio/format {:year 2020 :month 3 :day 6} [[:year 2] \. :month \. :day]))))
