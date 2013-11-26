;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.integral)

(defn inner-points
  "(a:Real b:Real N:Integer) -> Vector Real
  Return (N - 1) points equally spaced between a and b but not including a and b."
  [a b N]
  (let [diff (/ (- b a) N)]
    (rest (take N (iterate #(+ diff %) a)))))

(defn trapz
  "(f:(x:Real)->Real a:Real b:Real N:Real) -> Real
  Do a trapezoidal integration of f over a uniform grid
  from a to b with N steps."
  [f a b N]
  (let [h (/ (- b a) N)
        half-h (/ h 2)
        inner-pts (inner-points a b N)]
    (* half-h (+ (f a)
                 (f b)
                 (* 2 (apply + (map f inner-pts)))))))

(defn trapz-points
  "(f:(x:Real)->Real points:Seq Real) -> Real
  Do the trapezoidal integration"
  [f pts]
  (* 1/2 (apply + (map #(* (- %2 %1) (+ (f %1) (f %2))) pts (rest pts)))))
