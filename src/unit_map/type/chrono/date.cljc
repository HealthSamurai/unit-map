(ns unit-map.type.chrono.date
  (:require [unit-map.ops :as ops]
            [unit-map.io :as io]
            [unit-map.type.chrono.util.misc :as um]))


(def gregorian
  #unit-map/definition[:day    [1 2 .. um/days-in-month]
                       :month  [1 2 .. 12]
                       :year   [##-Inf .. -2 -1 1 2 .. ##Inf]])


(defmethod ops/definition ::gregorian [_] gregorian)


(def locale-month-en
  {1  {:full "January" :short "Jan" :regex "(?i)jan\\S*"}
   2  {:full "February" :short "Feb" :regex "(?i)feb\\S*"}
   3  {:full "March" :short "Mar" :regex "(?i)mar\\S*"}
   4  {:full "April" :short "Apr" :regex "(?i)apr\\S*"}
   5  {:full "May" :short "May" :regex "(?i)may\\S*"}
   6  {:full "June" :short "June" :regex "(?i)jun\\S*"}
   7  {:full "July" :short "July" :regex "(?i)jul\\S*"}
   8  {:full "August" :short "Aug" :regex "(?i)aug\\S*"}
   9  {:full "September" :short "Sep" :regex "(?i)sep\\S*"}
   10 {:full "October" :short "Oct" :regex "(?i)oct\\S*"}
   11 {:full "November" :short "Nov" :regex "(?i)nov\\S*"}
   12 {:full "December" :short "Dec" :regex "(?i)dec\\S*"}})


(def locale-month-ru
  {1 {:full "Январь", :short "Янв", :regex "(?iu)янв(ар(ь|я))?"}
   2 {:full "Февраль", :short "Фев", :regex "(?iu)фев(рал(ь|я))?"}
   3 {:full "Март", :short "Мар", :regex "(?iu)мар(та?)?"}
   4 {:full "Апрель", :short "Апр", :regex "(?iu)апр(ел(ь|я)?)?"}
   5 {:full "Май", :short "Май", :regex "(?iu)ма(й|я)?"}
   6 {:full "Июнь", :short "Июн", :regex "(?iu)июн(ь|я)?"}
   7 {:full "Июль", :short "Июл", :regex "(?iu)июл(ь|я)?"}
   8 {:full "Август", :short "Авг", :regex "(?iu)авг(уста?)?"}
   9 {:full "Сентябрь", :short "Сен", :regex "(?iu)сен(тябр(ь|я)?)?"}
   10 {:full "Октябрь", :short "Окт", :regex "(?iu)окт(ябр(ь|я)?)?"}
   11 {:full "Ноябрь", :short "Ноя", :regex "(?iu)ноя(бр(ь|я)?)?"}
   12 {:full "Декабрь", :short "Дек", :regex "(?iu)дек(бр(ь|я)?)?"}})


(defmethod io/field-settings :day [_]
  {:default-pad-width 2})


(defmethod io/field-settings :month [_]
  {:default-pad-width 2})


(defmethod io/field-locale [:month :default] [_ _] locale-month-en)


(defmethod io/field-locale [:month :en] [_ _] locale-month-en)


(defmethod io/field-locale [:month :ru] [_ _] locale-month-ru)


(defmethod io/field-settings :year [_]
  {:default-pad-width 4})
