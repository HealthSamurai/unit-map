(ns chrono.core
  (:require [chrono.ops :as ops]
            [chrono.now :as now]
            [chrono.io :as io])
  (:refer-clojure :exclude [+ - = > >= < <= not= format compare]))

(def parse io/parse)
(def format io/format)
(def strict-parse io/strict-parse)

(defn timestamp
  ([]  (timestamp (now/utc)))
  ([t] (io/to-epoch t)))

(defn diff [t t']) ; TODO

(def normalize ops/normalize)
(def to-utc ops/to-utc)
(def to-tz ops/to-tz)
(def to-normalized-utc ops/to-normalized-utc)

(def date-convertable? io/date-convertable?)
(def date-valid? io/date-valid?)

(def + ops/plus)
(def - ops/minus) ; TODO: add tests
(def = ops/eq?)
(def not= ops/not-eq?)
(def eq? ops/eq?)
(def > ops/gt)
(def >= ops/gte)
(def < ops/lt)
(def <= ops/lte)
(def compare ops/cmp)
