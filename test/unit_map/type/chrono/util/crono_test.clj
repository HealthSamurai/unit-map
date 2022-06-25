(ns unit-map.type.chrono.util.crono-test
  (:require [unit-map.type.chrono.util.crono :as sut]
            [unit-map.ops :as ops]
            [clojure.test :refer :all :as t]
            [unit-map.type.chrono.datetime :as datetime]))

(use-fixtures
  :each
  (fn [t]
    (defmethod ops/definition :default-type [_] datetime/gregorian-military)
    (t)))

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

  (is (t/is (= {:year 2020 :month 1 :day 1 :hour 12 :min 0}
               (sut/next-time {:year 2020 :month 1 :day 1 :hour 11 :min 43}
                              {:every :hour :at [{:min 0} {:min 30}]}))))

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

  (is (t/is (= {:year 2020 :month 1 :day 1 :hour 11 :min 0}
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
                                                 {:min 55}]}))))

  (is (t/is (= {:year 2020 :month 1 :day 1 :hour 12 :min 0}
               (sut/next-time {:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                              {:every :hour}))))

  (is (t/is (= {:year 2020 :month 1 :day 1 :hour 11 :min 10 :sec 0}
               (sut/next-time {:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                              {:every :min}))))

  (is (t/is (= {:year 2020 :month 1 :day 1 :hour 11 :min 10 :sec 0}
               (sut/next-time {:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                              {:every :min :at {:sec 0}}))))

  (is (= {:year 2020 :month 1 :day 1 :hour 11 :min 10 :sec 10}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 11 :min 9 :sec 10}
                        {:every :min :at {:sec 10}})))


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

  (is (= {:year 2020 :month 1 :day 1 :hour 12}
         (sut/next-time {:year 2020 :month 1 :day 1 :hour 11}
                        {:every "day" :at {:hour 12}})))

  (is (= {:year 2020 :month 5 :day 18 :hour 10}
         (sut/next-time {:year 2020 :month 5 :day 17 :hour 9}
                        {:every "monday" :at {:hour 10}})))

  (is (= {:year 2020 :month 5 :day 18 :hour 10}
         (sut/next-time {:year 2020 :month 5 :day 18 :hour 9}
                        {:every "monday" :at {:hour 10}})))

  (is (= {:year 2020 :month 5 :day 26 :hour 10}
         (sut/next-time {:year 2020 :month 5 :day 19 :hour 11}
                        {:every "tuesday" :at {:hour 10}})))

  (is (= {:year 2020 :month 5 :day 19 :hour 10}
         (sut/next-time {:year 2020 :month 5 :day 18 :hour 11}
                        {:every "tuesday" :at {:hour 10}})))

  )
