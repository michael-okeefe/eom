;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.numeric-test
  (:use [clojure.test :only (deftest is are)])
  (:use [midje.sweet])
  (:require [eom.numeric :as numeric]))

(deftest not-neg?
  (is (= true (numeric/not-neg? 3)))
  (is (= false (numeric/not-neg? -4.5))))

(deftest abs
  (is (= 4 (numeric/abs -4)))
  (is (= 1/2 (numeric/abs -1/2)))
  (is (== 3 (numeric/abs 3M)))
  (is (= 4 (numeric/abs 4))))

(deftest square
  (is (= 4 (numeric/square 2)))
  (is (= 16 (numeric/square 4)))
  (is (= 1/64 (numeric/square 1/8))))

(deftest cube
  (is (= 27 (numeric/cube 3)))
  (is (= 64 (numeric/cube 4))))

(deftest sum
  (is (= 3 (numeric/sum [0 1 2])))
         (is (= 0 (numeric/sum [])))
  (is (= 1 (numeric/sum [0 -1 -2 4]))))

(deftest diff
  (is (= [1 1 1] (numeric/diff [1 2 3 4])))
  (is (= [0 1 2 3] (numeric/diff [1 1 2 4 7])))
  (is (= [] (numeric/diff [])))
  (is (= [] (numeric/diff [1]))))

(deftest map-self
  (is (= [1 2 2 4] (numeric/map-self - [1 2 4 6 10]))))

(deftest add-ea
  (is (= [2 4 6] (numeric/add-ea [1 2 3] [1 2 3])))
  (is (= [] (numeric/add-ea [] [])))
  (is (= [5] (numeric/add-ea [3] [2])))
  (is (= [3 7] (numeric/add-ea [2 6 10 20] [1 1]))))

(deftest sub-ea
  (is (= [1 -3 -6] (numeric/sub-ea [2 10 10] [1 13 16]))))

(deftest mul-ea
  (is (= [0 2 4] (numeric/mul-ea [2 6 -9] [0 1/3 -4/9]))))

(deftest div-ea
  (is (= [2 2 2] (numeric/div-ea [10 20 30] [5 10 15]))))

(deftest safe-div
  (is (== 0 (numeric/safe-div 0 0.0 200.0)))
  (is (== 0 (numeric/safe-div 0 100.0 0.0)))
  (is (== 10 (numeric/safe-div 0 100 10))))

(deftest safe-div-ea
  (is (= [2 2 2] (numeric/safe-div-ea 0 [10 20 30] [5 10 15])))
  (is (= [2 0 2] (numeric/safe-div-ea 0 [10 20 30] [5  0 15]))))

(deftest pow-ea
  (is (= [9.0 16.0 25.0] (numeric/pow-ea [3 4 5] 2))))

(fact "binary-average of 2 and 6 is 4"
      (numeric/binary-average 2 6)
      => 4.0)

(fact "averages"
      (numeric/averages [])
      => []
      (numeric/averages [1 2 3 4])
      => [1.5 2.5 3.5])

(fact "average2s"
      (numeric/average2s [1.0 2.0])
      => [(/ 7.0 3.0)]
      (numeric/average2s [1 2])
      => [(/ 7.0 3.0)]
      (numeric/average2s [2 2 2 2])
      => [4.0 4.0 4.0])

(fact "binary-average3 of a constant line segment is the cube of the constant value"
      (numeric/binary-average3 3 3)
      => 27.0)

(fact "average3s"
      (numeric/average3s [1.0 2.0])
      => [(/ 15.0 4.0)]
      (numeric/average3s [1 2])
      => [(/ 15.0 4.0)]
      (numeric/average3s [2 2 2 2])
      => [8.0 8.0 8.0])

(deftest cumsum
  (is (= [] (numeric/cumsum [])))
  (is (= [1 3 6] (numeric/cumsum [1 2 3]))))

(deftest positive
  (is (== 6 (numeric/positive 6)))
  (is (== 0 (numeric/positive -6))))

(deftest negative
  (are [n1 n2] (== n1 n2)
       -3 (numeric/negative -3)
       0 (numeric/negative 3)))

(deftest select-by-mask
  (is (= [2 4 6] (numeric/select-by-mask [1 2 3 4 5 6 7 8]
                                         [false true false true
                                          false true false false]))))

(deftest increasing?
  (is (numeric/increasing? [1 2 3 4]))
  (is (numeric/increasing? [-10 -4 10]))
  (is (not (numeric/increasing? [3 2 1])))
  (is (not (numeric/increasing? [1 2 3 3 4]))))
