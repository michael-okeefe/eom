;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; References:
;; Cavallo, A.J. (2004). "Hubbert’s Petroleum Production Model: An Evaluation
;; and Implications for World Oil Production Forecasts". Natural Resources
;; Research, Vol. 13, No. 4, December 2004
(ns eom.hubbert)

;; Cavallo 2004 equation 1
(defn cumulative-production
  "(t:Real A:Real a:Real b:Real) -> Real
  The cumulative production of a resource as a function of elapsed time (t),
  total available resource (A), and two fitting constants, a and b.
  Note that b should be negative."
  [t A a b]
  (/ A (+ 1.0 (* a (Math/exp (* b t))))))

;; Cavallo 2004 equation 3
(defn time-of-max-production-rate
  "(a:Real b:Real) -> Real
  The elapsed time when the production rate is at its peak as
  a function of the fitting constants a and b."
  [a b]
  (* (/ b) (Math/log (/ a))))

;; derivative of the cumulative-production equation
(defn production-rate
  "(t:Real A:Real a:Real b:Real) -> Real
  The production rate per one unit of time of a resource per
  Hubbert's model using an ultimate available amount of the
  resource, A, and two fitting coefficients, a and b."
  [t A a b]
  (let [a-exp-bt (* a (Math/exp (* b t)))]
    (/ (* -1.0 A b a-exp-bt)
       (Math/pow (+ 1.0 a-exp-bt) 2.0))))
