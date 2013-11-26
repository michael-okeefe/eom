;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.integral-test
  (:use [midje.sweet])
  (:require [eom.integral :as in]))

(fact "The 3 inner points for 3 transitions between 0 and 3 are 1 and 2"
      (in/inner-points 0 3 3) => [1 2])

(fact "The trapezoidal integration of the Sin from 0 to PI is about 2"
      (in/trapz (fn [x] (Math/sin x)) 0.0 Math/PI 1000.0) => (roughly 2.0)
      (in/trapz (fn [x] x) 0 2 10) => 2)

(fact "The trapezoidal integration by points for Sin from 0 to PI is about 2"
      (let [h (/ Math/PI 1000.0)]
        (in/trapz-points (fn [x] (Math/sin x)) (range 0.0 (+ Math/PI h) h))
        => (roughly 2.0))
      (in/trapz-points (fn [x] x) [0 2]) => 2)
