;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.hubbert-test
  (:use [midje.sweet])
  (:require [eom.hubbert :as hub])
  (:require [eom.integral :as in]))

(fact "Integral to the Hubbert Peak is 1/2 the resource"
      (let [A 170.0
            a 46.8
            b -0.0687]
        (in/trapz #(hub/production-rate % A a b)
                  -1000.0
                  (hub/time-of-max-production-rate a b)
                  10000)
        => (roughly (* 0.5 A))))

(fact "Time of Hubbert's peak production ~= 56 yrs"
      (hub/time-of-max-production-rate 46.8 -0.0687)
      => (roughly 56.0 5/100))
