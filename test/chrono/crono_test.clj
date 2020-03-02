(ns chrono.crono-test
  (:require [chrono.crono :as sut]
            [clojure.test :refer :all]))

(deftest crono-test


  (is (= {:year 2020 :month 1 :day 1 :hour 12}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 11}
                  {:every :day :at {:hour 12}})))

  (is (= {:year 2020 :month 1 :day 2 :hour 12}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 12 :min 10}
                    {:every :day :at {:hour 12}})))

  (is (= {:year 2020 :month 1 :day 1 :hour 14}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 13}
                      {:every :day :at [{:hour 12}
                                        {:hour 14}]})))

  (is (= {:year 2020 :month 1 :day 1 :hour 10 :min 30}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 10 :min 13}
                      {:every :hour :at [{:min 0} {:min 30}]})))

  (is (= {:year 2020 :month 1 :day 1 :hour 12}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 11 :min 43}
                      {:every :hour :at [{:min 0} {:min 30}]})))

  (is (= {:year 2020 :month 1 :day 1 :hour 12 :min 10}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 12 :min 7}
                    {:every :hour :at [{:min 0}
                                       {:min 5}
                                       {:min 10}
                                       {:min 15}
                                       {:min 20}
                                       {:min 25}
                                       {:min 30}
                                       {:min 35}
                                       {:min 40}
                                       {:min 45}
                                       {:min 50}
                                       {:min 55}]})))

  (is (= {:year 2020 :month 1 :day 1 :hour 11}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 10 :min 55}
                    {:every :hour :at [{:min 0}
                                       {:min 5}
                                       {:min 10}
                                       {:min 15}
                                       {:min 20}
                                       {:min 25}
                                       {:min 30}
                                       {:min 35}
                                       {:min 40}
                                       {:min 45}
                                       {:min 50}
                                       {:min 55}]})))

  (is (= true
         (sut/now? {:year 2020 :month 1 :day 1 :hour 12 :min 1}
                   {:every :day
                    :at {:hour 12}
                    :until {:hour 12 :min 30}})))

  (is (= false
         (sut/now? {:year 2020 :month 1 :day 1 :hour 12 :min 31}
                   {:every :day
                    :at {:hour 12}
                    :until {:hour 12 :min 30}})))

  (is (= true
         (sut/now? {:year 2020 :month 1 :day 1 :hour 12 :min 31}
                   {:every :day
                    :at {:hour 12}})))

  )
