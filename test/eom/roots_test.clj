;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.roots-test
  (:use [midje.sweet])
  (:require [eom.roots :as roots]))

(fact "inverse-quadratic-interpolation"
      (roots/guess-root-by-inverse-quadratic-interpolation
       0.0 -2.0 4.0 3.0 2.0 2.0)
      => (roughly -0.2 1e-3)
      (roots/guess-root-by-inverse-quadratic-interpolation
       0.0 -2.0 2.0 1.0 5.0 3.0)
      => (roughly 1.0 1e-3))

(fact "f(x) = cos(x) - x**3"
      (let [f (fn [x] (- (Math/cos x) (* x x x)))]
        (:root (roots/bisection f -10.0 10.0 1e-6))
        => (roughly 0.865 1e-3)
        (:root (roots/regula-falsi f -10.0 10.0 1e-6))
        => (roughly 0.865 1e-3)
        (:root (roots/brent-simplified f -10.0 10.0 1e-6))
        => (roughly 0.865 1e-3)
        (:root (roots/brent f -10.0 10.0 1e-6))
        => (roughly 0.865 1e-3)
        (:root (roots/zhang f -10.0 10.0 1e-6))
        => (roughly 0.865 1e-3)))

(fact "f(x) = 2 * x**2 - 8"
      (let [f (fn [x] (- (* 2.0 x x) 8.0))]
        (:root (roots/bisection f 0.0 10.0 1e-6))
        => (roughly 2.0 1e-3)
        (:root (roots/regula-falsi f 0.0 10.0 1e-6))
        => (roughly 2.0 1e-3)
        (:root (roots/brent-simplified f 0.0 10.0 1e-6))
        => (roughly 2.0 1e-3)
        (:root (roots/brent f 0.0 10.0 1e-6))
        => (roughly 2.0 1e-3)
        (:root (roots/zhang f 0.0 10.0 1e-6))
        => (roughly 2.0 1e-3)))
