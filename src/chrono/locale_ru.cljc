(ns chrono.locale-ru
  (:require [chrono.util :as util]))

(defmethod util/locale :ru [_]
  {:month
   {1  {:name "Январь" :short "Янв" :regex "(?iu)янв(арь|аря)"}
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
