(ns chrono.locale-en
  (:require [chrono.util :as util]))

(def locale-en
  {:month
   {1  {:name "January" :short "Jan" :regex "(?i)jan\\S*"}
    2  {:name "February" :short "Feb" :regex "(?i)feb\\S*"}
    3  {:name "March" :short "Mar" :regex "(?i)mar\\S*"}
    4  {:name "April" :short "Apr" :regex "(?i)apr\\S*"}
    5  {:name "May" :short "May" :regex "(?i)may\\S*"}
    6  {:name "June" :short "June" :regex "(?i)jun\\S*"}
    7  {:name "July" :short "July" :regex "(?i)jul\\S*"}
    8  {:name "August" :short "Aug" :regex "(?i)aug\\S*"}
    9  {:name "September" :short "Sep" :regex "(?i)sep\\S*"}
    10 {:name "October" :short "Oct" :regex "(?i)oct\\S*"}
    11 {:name "November" :short "Nov" :regex "(?i)nov\\S*"}
    12 {:name "December" :short "Dec" :regex "(?i)dec\\S*"}}})

(defmethod util/locale :en [_] locale-en)
(defmethod util/locale :default [_] locale-en)
