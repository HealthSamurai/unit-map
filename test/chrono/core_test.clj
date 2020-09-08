(ns chrono.core-test
  (:require [clojure.test :refer :all]
            [matcho.core :as matcho]
            [chrono.type.datetime :as datetime]
            [chrono.ops :as ops]
            [chrono.core :as sut]))

(use-fixtures
  :each
  (fn [t]
    (defmethod ops/definition :default-type [_] datetime/gregorian-military)
    (t)))

(deftest from-epoch-test
  (is (= {:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1} (sut/from-epoch 1587520180)))
  (is (= {:day 1, :month 1, :year 1970} (sut/from-epoch 0))))

(deftest to-epoch-test
  (is (= 1587520180 (sut/to-epoch {:day 22, :month 4, :year 2020, :sec 40, :min 49, :hour 1})))
  (is (= 0 (sut/to-epoch {:day 1, :month 1, :year 1970, :sec 0, :min 0, :hour 0}))))
