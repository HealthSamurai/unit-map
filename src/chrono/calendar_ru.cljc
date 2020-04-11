(ns chrono.calendar-ru
  (:require [chrono.calendar :refer [month-names]]))

(defmethod month-names :ru [_]
  {1  {:name "Январь"   }
   2  {:name "Февраль"  }
   3  {:name "Март"     }
   4  {:name "Апрель"   }
   5  {:name "Май"      }
   6  {:name "Июнь"     }
   7  {:name "Июль"     }
   8  {:name "Август"   }
   9  {:name "Сентябрь" }
   10 {:name "Октябрь"  }
   11 {:name "Ноябрь"   }
   12 {:name "Декабрь"  }})
