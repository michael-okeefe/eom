;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.deriv)

(defn deriv
  "(f:(x:Number)->Number x:Number dx:Number) -> Number
  Takes the numerical derivative of f at x using a step of dx.
  Requires 2 calls to f."
  [f x dx]
  (/ (- (f (+ x dx)) (f x))
     dx))

(defn deriv-3-call
  "(f:(x:Number)->Number x:Number dx:Number) -> Number
  Takes the derivative of f at x using a step of dx but balanced
  between dx forward and back. Requires 3 calls to f."
  [f x dx]
  (let [fx-dx (f (- x dx))
        fx (f x)
        fx+dx (f (+ x dx))
        der-bck (/ (- fx fx-dx) dx)
        der-fwd (/ (- fx+dx fx) dx)]
    (/ (+ der-fwd der-bck) 2.0)))

(defn deriv-4-call
  "(f:(x:Number)->Number x:Number dx:Number) -> Number
  Takes the derivative of f at x using the 5 point stencil
  algorithm. Requires 4 calls to f."
  [f x dx]
  (let [[f-2 f-1 f1 f2] (map f [(- x dx dx) (- x dx) (+ x dx) (+ x dx dx)])]
    (/ (+ f-2 (* -8.0 f-1) (* 8.0 f1) (* -1.0 f2))
       (* dx 12.0))))

(deriv (fn [x] (Math/sin x)) (/ Math/PI 4.0) 1/1000)
(deriv-3-call (fn [x] (Math/sin x)) (/ Math/PI 4.0) 1/1000)
(deriv-4-call (fn [x] (Math/sin x)) (/ Math/PI 4.0) 1/1000)
(Math/cos (/ Math/PI 4.0))
