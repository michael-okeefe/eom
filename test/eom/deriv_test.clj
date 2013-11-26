;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.deriv-test
  (:use [midje.sweet])
  (:require [eom.deriv :as der]))

(fact "Numerical derivative of sin(PI/4) is cos(PI/4)"
      (let [f (fn [x] (Math/sin x))
            x (/ Math/PI 4.0)
            dx 1/1000
            ans (Math/cos x)]
        (der/deriv f x dx) => (roughly ans)
        (der/deriv-3-call f x dx) => (roughly ans)
        (der/deriv-4-call f x dx) => (roughly ans)))
