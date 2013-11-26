;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.unit-conversion-test
  (:use [midje.sweet])
  (:require [eom.unit-conversion :as UC])
  (:use [eom.unit-conversion :only [parse-length]])
  (:use [clojure.test :only (deftest is are)]))

(UC/def-units-of letter :a "A fake unit of letters"
   :a #{:A}
   :b 10
   :b #{:B :bee}
   :c [10 :b]
   :d [10 :c]
   :e [10 :d])

(deftest test-letter-convert
  (is (== 10 (convert-letter 1 [1 :b] [1 :a]))))

(deftest test-letter-table
  (is (= letter-conversion-table {:a 1 :A :a :b 10 :B :b :bee :b :c [10 :b] :d [10 :c] :e [10 :d]})))

(deftest test-letter-conversions
  (are [x y] (= x y)
       (letter 1 :a) 1
       (letter 1 :b) 10
       (letter 1 :c) 100
       (letter 1 :d) 1000
       (letter 1 :e) 10000))

(deftest test-parse-letter
  (are [x y] (= x y)
       (parse-letter [5 :a 2 :b]) 25
       (parse-letter [1 :e 2 :d 3 :c 4 :b 5 :a]) 12345))

(deftest test-length
  (are [x y] (== x y)
       (UC/convert-length 1 [1 :km] [1 :cm]) 100000
       (UC/convert-length 10 [1 :km] [100 :km]) 0.1
       (UC/length 1 :m) 1
       (UC/length 1 :km) 1000
       (UC/length 1 :mile) (* 5280 12 254/100 1/100)))

(deftest test-area
  (are [x y] (= x y)
       (UC/area 1 :acre) (* 43560 1200/3937 1200/3937)))

(deftest test-volume
  (are [x y] (= x y)
       (UC/volume 1 :L) 1/1000
       (UC/volume 1 :cm3) 1/1000000))

(fact "Units of temperature difference"
      (UC/temperature-difference 2 :K) => 2
      (UC/temperature-difference 9 :R) => 5
      (UC/convert-temperature-difference 2 [1 :K] [1 :C]) => 2)

(fact "Units of absolute temperature"
      (UC/absolute-temperature 3 :K) => 3
      (UC/absolute-temperature 9 :R) => 5
      (UC/convert-absolute-temperature 9 [1 :R] [1 :K]) => 5)
