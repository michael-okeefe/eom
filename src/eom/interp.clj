;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; # Interpolation
;;
;; This namespace contains functions required for interpolation.
(ns eom.interp
  (:use [eom.numeric :only [increasing?]]))

(defn mk-model
  "seq-of-number seq-of-number -> model
  Return a linear interpolation model."
  [xs ys]
  {:pre [(= (count xs) (count ys))
         (increasing? xs)
         (every? number? xs)
         (every? number? ys)]}
  {:xs xs :ys ys})

(defn- mk-interp-1d-linear-reducer
  "let n be a number =>
  n -> (n|nil [n n n n] -> n|nil)
  Returns a function that returns the interpolated number
  if x is within range [x1 x2], otherwise nil"
  [x]
  (fn [value [x1 x2 y1 y2]]
    (if (not (nil? value))
      value
      (if (and (>= x x1) (<= x x2))
        (let [dx (- x2 x1)
              dy (- y2 y1)
              delta (- x x1)]
          (+ y1 (* (/ delta dx) dy)))
        nil))))

(defn linear-1d
  "model number -> float|nil
  Return the 'y' for the given 'x' using linear
  interpolation assuming the given one-dimensional
  model. Returns nil if x not in the range of the
  model."
  [model x]
  (let [{:keys [xs ys]} model]
    (reduce (mk-interp-1d-linear-reducer x)
            nil
            (map vector xs (rest xs) ys (rest ys)))))

(defn with-edge-extension
  "(model number -> number|nil) model number -> number
  Return the value y for the given lookup function, model, and x.
  When x is out of interpolation range, use the last known
  good value (i.e., edge-extension)."
  [lookup model x]
  (let [{:keys [xs ys]} model]
    (cond (> x (last xs)) (last ys)
          (< x (first xs)) (first ys)
          :else (lookup model x))))
