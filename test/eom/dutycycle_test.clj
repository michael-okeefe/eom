;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.dutycycle-test
  (:use [midje.sweet])
  (:require [eom.dutycycle :as dutycycle])
  (:use [eom.test-utils :only [*tolerance* approx=s]])
  (:use [clojure.test :only [deftest is are]]))

(def dc-01
  (let [xs (range 0 4)
        ys [0 1/2 2 9/2]]
    (dutycycle/mk-dutycycle "01" xs xs ys)))

(def dc-02
  (dutycycle/mk-dutycycle "02" [0 1 2 3] [2 2 2 2] [0 0 0 0]))

(def dc-03
  (assoc dc-02 :speeds-m--s [0 1 2 3]))

(def dc-04
  (dutycycle/mk-dutycycle "04"
              [0 1 2 3 4 5 6 7 8 9]
              [0 1 2 1 0 1 0 2 1 0]
              [0 0 0 0 0 0 0 0 0 0]))

(def dc-05
  (let [xs (range 11)]
    (dutycycle/mk-dutycycle
      "05"
      (map double xs)
      (map #(if (> % 5) (double (- 10 %)) (double %)) xs)
      (map #(if (> % 5) (double (/ (- 10 %) 10.0)) (* % 0.1)) xs))))

(deftest duty-cycle-equality
  (are [x y] (= x y)
       (:name dc-01) "01"
       (:speeds-m--s dc-01) [0.0 1.0 2.0 3.0]
       (:times-s dc-01) [0.0 1.0 2.0 3.0]
       (:elevations-m dc-01) [0.0 0.5 2.0 4.5]))

(deftest num-samples
  (is (== 11 (dutycycle/num-samples
               dutycycle/example-dutycycle))))

(deftest num-transitions
  (is (== 10 (dutycycle/num-transitions
               dutycycle/example-dutycycle))))

(deftest total-time-s
  (is (== 3 (dutycycle/total-time-s dc-01))))

(deftest time-steps-s
  (is (= [1.0 1.0 1.0] (dutycycle/time-steps-s dc-01))))

(deftest speed-diffs-m--s
  (is (= [1.0 1.0 1.0] (dutycycle/speed-diffs-m--s dc-01))))

(deftest elevation-diffs-m
  (is (= [0.5 1.5 2.5] (dutycycle/elevation-diffs-m dc-01))))

(deftest accelerations-m--s2
  (is (= [1.0 1.0 1.0] (dutycycle/accelerations-m--s2 dc-01))))

(deftest average-speeds-m--s
  (is (= [0.5 1.5 2.5] (dutycycle/average-speeds-m--s dc-01))))

(deftest climbing-rates-m--s
  (binding [*tolerance* 0.001]
    (are [xs ys] (approx=s xs ys)
         [0.1 0.1 0.1 0.1 0.1 -0.1 -0.1 -0.1 -0.1 -0.1]
         (dutycycle/climbing-rates-m--s dc-05)
         [0.1 -0.1]
         (dutycycle/climbing-rates-m--s
          (dutycycle/mk-dutycycle
           ""
           [0.0 10.0 20.0]
           [0.0 10.0 20.0]
           [0.0 1.0 0.0])))))

(deftest average-speeds-cubed-m3--s3
  (is (= [1.0 1.0 1.0]
         (dutycycle/average-speeds-cubed-m3--s3
           (dutycycle/mk-dutycycle
             "const" [0 1 2 3] [1.0 1.0 1.0 1.0] [0 0 0 0])))))

(deftest speeds2-difference-m2--s2
  (is (= [0.0 0.0 0.0]
         (dutycycle/speeds2-difference-m2--s2
           (dutycycle/mk-dutycycle
             "const" [0 1 2 3] [1.0 1.0 1.0 1.0] [0 0 0 0])))))

(deftest distance-diffs-m
  (is (= [0.5 1.5 2.5] (dutycycle/distance-diffs-m dc-01))))

(deftest distances-m
  (is (= [0.0 0.5 2.0 4.5] (dutycycle/distances-m dc-01))))

(deftest distance-m
  (is (== 9/2 (dutycycle/distance-m dc-01))))

(deftest average-speed-m--s
  (is (== 2 (dutycycle/average-speed-m--s dc-02))))

(deftest moving-time-s
  (is (== 3 (dutycycle/moving-time-s dc-01))))

(deftest stopped-time-s
  (is (== 0 (dutycycle/stopped-time-s dc-01))))

(deftest char-accels-m--s2
  (is (= [0.0 0.0 0.0]
         (dutycycle/characteristic-accelerations-m--s2 dc-02))))

(deftest aero-speed2s-m2--s2
  (is (= [4.0 4.0 4.0]
         (dutycycle/aerodynamic-speed2s-m2--s2 dc-02))))

(deftest aerodynamic-speeds-m--s
  (is (= [2.0 2.0 2.0] (dutycycle/aerodynamic-speeds-m--s dc-02))))

(deftest characteristic-acceleration-m--s2
  (is (== 1 (dutycycle/characteristic-acceleration-m--s2 dc-03))))

(deftest aerodynamic-speed2-m2--s2
  (is (== 4 (dutycycle/aerodynamic-speed2-m2--s2 dc-02))))

(deftest aerodynamic-speed-m--s
  (is (== 2 (dutycycle/aerodynamic-speed-m--s dc-02))))

(deftest microtrips
  (is (== 3 (count (dutycycle/microtrips dc-04))))
  (is (= [0.0 1.0 2.0 1.0 0.0] (:speeds-m--s
                                (first (dutycycle/microtrips dc-04))))))

(deftest splice
  (is (= (dutycycle/mk-dutycycle "big one"
                                  [0 1 2 3 4 5 6]
                                  [0 1 0 1 0 1 0]
                                  [0 0 0 0 0 0 0])
         (dutycycle/splice
          [(dutycycle/mk-dutycycle "big one"
                                   [0 1 2]
                                   [0 1 0]
                                   [0 0 0])
           (dutycycle/mk-dutycycle ""
                                   [0 1 2]
                                   [0 1 0]
                                   [0 0 0])
           (dutycycle/mk-dutycycle ""
                                   [0 1 2]
                                   [0 1 0]
                                   [0 0 0])]))))

(deftest mk-dutycycle
  (is (try
         (dutycycle/mk-dutycycle "a" [0 1 2 3] [0 1 2 3] [0 5 0])
         false
         (catch Exception e true)))
  (is (try
         (dutycycle/mk-dutycycle "a"
                                  [0.0 1.0 2.0 3.0]
                                  [0.0 1.0 2.0 3.0]
                                  [0.0 0.0 0.0 0.0])
         true
         (catch Exception e false)))
  (is (try
        (dutycycle/mk-dutycycle "b" [0 1 2 3] [0 1 2 3] [5 0 5])
        false
        (catch Exception e true))))

(fact "Microtrips and Splice are Idempotent"
      (let [cyc (dutycycle/mk-dutycycle "big one"
                                        [0 1 2 3 4 5 6]
                                        [0 1 0 1 0 1 0]
                                        [0 0 0 0 0 0 0])]
        (dutycycle/splice (dutycycle/microtrips cyc))
        => cyc))
