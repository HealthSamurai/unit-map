(ns unit-map.systems.date-time.misc-test
  (:require [unit-map.systems.date-time.misc :as sut]
            [unit-map.core :as umap]
            [clojure.test :as t]))

(def treg_ (umap/new-registry))

(umap/reg-useq! treg_ :unit :sec,  :useq #unit-map/useq[0 1 .. 59] :next-unit :min)
(umap/reg-useq! treg_ :unit :min,  :useq #unit-map/useq[0 1 .. 59] :next-unit :hour)
(umap/reg-useq! treg_ :unit :hour, :useq #unit-map/useq[0 1 .. 23] :next-unit :day)

(umap/reg-useq! treg_ :unit :day,   :useq #unit-map/useq[1 2 .. sut/days-in-month] :next-unit :month)
(umap/reg-useq! treg_ :unit :month, :useq #unit-map/useq[1 2 .. 12] :next-unit :year)
(umap/reg-useq! treg_ :unit :year,  :useq #unit-map/useq[##-Inf .. -2 -1 1 2 .. ##Inf])

(umap/reg-system! treg_ [:sec :min :hour :day :month :year])

(t/deftest from-epoch-test
  (t/is (= {:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1}
           (sut/from-epoch @treg_ 1587520180)))

  (t/is (= {:day 1, :month 1, :year 1970}
           (sut/from-epoch @treg_ 0))))

(t/deftest to-epoch-test
  (t/is (= 1587520180
           (sut/to-epoch {:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1})))

  (t/is (= 0
           (sut/to-epoch {:day 1, :month 1, :year 1970, :sec 0, :min 0, :hour 0}))))
