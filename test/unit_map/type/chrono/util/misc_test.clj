(ns unit-map.type.chrono.util.misc-test
  (:require [unit-map.type.chrono.util.misc :as sut]
            [unit-map.type.chrono.datetime :as datetime]
            [unit-map.ops :as ops]
            [clojure.test :as t]))


(t/use-fixtures
  :each
  (fn [t]
    (defmethod ops/definition :default-type [_] datetime/gregorian-military)
    (t)))

(t/deftest from-epoch-test
  (t/is (= {:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1} (sut/from-epoch 1587520180)))
  (t/is (= {:day 1, :month 1, :year 1970} (sut/from-epoch 0))))

(t/deftest to-epoch-test
  (t/is (= 1587520180 (sut/to-epoch {:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1})))
  (t/is (= 0 (sut/to-epoch {:day 1, :month 1, :year 1970, :sec 0, :min 0, :hour 0}))))
