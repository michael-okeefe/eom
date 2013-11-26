;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.numeric)

(defn not-neg?
  "[number] -> bool
  True if not negative (i.e., zero or positive), false otherwise."
  [n]
  (not (neg? n)))

(defn abs
  "[number] -> number
  Absolute value of a number"
  [n]
  (if (< n 0)
    (- n)
    n))

(defn square
  "[number] -> number
  Square of the argument"
  [n]
  (* n n))

(defn cube
  "[number] -> number
  Cube the argument"
  [n]
  (* n n n))

(defn sum
  "[seq-of-number] -> number
  Sum of the elements in the sequence"
  [xs]
  (reduce + 0 xs))

(defn map-self
  "for any x, y => [([x-j+1 x-j] -> y) seq-of-x] -> seq-of-y
  Maps function, f, over two sequences: the rest of xs and xs itself. This
  is convenient for time series data where functions are applied stepwise
  to the transitions between sample points and a transition function requires
  the sample at start and end of the step transition."
  [f xs]
  (map f (rest xs) xs))

(defn diff
  "[seq-of-number] -> seq-of-number
  The element-wise difference of the sequence"
  [xs]
  (map-self - xs))

(defn add-ea
  "[seq-of-number ...] -> seq-of-number
  Add multiple sequences of numbers element-wise"
  [& xss]
  (apply (partial map +) xss))

(defn sub-ea
  "[seq-of-number ...] -> seq-of-number
  Subtract multiple numeric sequences element-wise"
  [& xss]
  (apply (partial map -) xss))

(defn div-ea
  "[seq-of-number ...] -> seq-of-number
  Divide multiple numeric sequences element-wise"
  [& xss]
  (apply (partial map /) xss))

(defn safe-div
  "[number number ...] -> number
  Perform a normal division unless one of the denominators is zero, in
  which case, the first element is returned. Otherwise, the first
  element is ignored and safe-div acts like '/' applied to all but the
  first argument."
  [if-zero & xs]
  (if (or (and (= 1 (count xs)) (== 0 (first xs)))
          (some #(== 0 %) (rest xs)))
    if-zero
    (apply / xs)))

(defn safe-div-ea
  "[number seq-of-number ...] -> seq-of-number
  Divide elements of argument sequences except where the denominator
  is zero. When the denominator is zero, return the if-zero value
  (first argument)."
  [if-zero & xss]
  (apply (partial map (partial safe-div if-zero)) xss))

(defn mul-ea
  "[seq-of-number ...] -> seq-of-number
  Multiply numerical sequences element-wise"
  [& xss]
  (apply (partial map *) xss))

(defn pow-ea
  "[seq-of-number number] -> seq-of-number
  Raise each element of a numerical sequence to the given power"
  [xs n]
  (map #(java.lang.Math/pow % n) xs))

(defn binary-average
  "(x1:Number * x2:Number) -> Number
  Return the average of two numbers"
  [x1 x2]
  (/ (+ x1 x2) 2.0))

(defn averages
  "[seq-of-number] -> seq-of-number
  The step-wise average over a numerical sequence"
  [xs]
  (map-self binary-average xs))

(defn binary-average2
  "(x1:Number * x2:Number) -> Number
  Return the average of the square of two numbers
  that define a line segment from x1 to x2"
  [x1 x2]
  (/ (+ (* x1 x1)
        (* x1 x2)
        (* x2 x2))
     3.0))

(defn average2s
  "[seq-of-number] -> seq-of-number
  The step-wise average of the squares of the numerical sequence
  assuming linear transition between sample points."
  [xs]
  (map-self binary-average2 xs))

(defn binary-average3
  "(x1:Number * x2:Number) -> Number
  Returns the average of the cube of the arguments assuming
  that there is a linear change between x and y"
  [x1 x2]
  (/ (+ (* x1 x1 x1)
            (* x1 x1 x2)
            (* x1 x2 x2)
            (* x2 x2 x2))
     4.0))

(defn average3s
  "[seq-of-number] -> seq-of-number
  The step-wise average of the cube of the numerical sequence
  assuming linear transition between sample points."
  [xs]
  (map-self binary-average3 xs))

(defn cumsum
  "[seq-of-number] -> seq-of-number
  The cumulative sum of the numeric sequence."
  [xs]
  (reduce (fn [a-vec x]
            (let [y (if (empty? a-vec) 0 (last a-vec))]
              (conj a-vec (+ x y))))
          []
          xs))

(defn positive
  "[number] -> number
  Return the number if positive, otherwise 0.0"
  [x]
  (if (pos? x) x 0.0))

(defn negative
  "[number] -> number
  Return the number if negative, else 0.0"
  [n]
  (if (neg? n) n 0.0))

(defn select-by-mask
  "for any type a => [seq-of-a seq-of-bool] -> seq-of-b
  Select xs where the corresponding element in flags is true
  where flags is a sequence of booleans"
  [xs flags]
  (reduce (fn [ys [x flag]] (if flag (conj ys x) ys))
          []
          (map (fn [x y] [x y]) xs flags)))

(defn increasing?
  [xs]
  "seq-of-number -> bool
  Return true if the sequence of numbers is always increasing,
  otherwise false"
  (reduce (fn [flag [x1 x2]] (and flag (> x2 x1)))
          (map vector xs (rest xs))))
