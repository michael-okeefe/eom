;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.test-utils)

(def ^:dynamic *tolerance* 1e-4)

(defn approx= [x y]
  "[number number] -> bool
  Report if numbers are equal within tolerance
  * Parameters: *tolerance* :: number"
  (> *tolerance* (Math/abs (- x y))))

(defn approx=s
  "[seq-of-number seq-of-number] -> bool
  Report if two sequences of numbers are equal within tolerance
  * Parameters: *tolerance* :: number"
  [xs ys]
  (and (= (count xs) (count ys))
       (every? true?
               (map approx= xs ys))))
