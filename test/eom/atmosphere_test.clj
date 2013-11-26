;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.atmosphere-test
  (:require [eom.constants :as const]
            [eom.atmosphere :as std])
  (:use [clojure.test :only (deftest is)]))

(deftest std-temperature
  (is (= (std/std-temperature-K 0)
         const/sea-level-standard-temperature-K)))

(deftest std-pressure
  (is (= (std/std-pressure-kPa 0)
         const/sea-level-standard-atmospheric-pressure-kPa)))

(deftest std-density
  (is (< (Math/abs (- 1.225 (std/std-air-density-kg--m3 0.0))) 1e-4)))
