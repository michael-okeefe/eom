;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.vehicle-test
  (:require [eom.vehicle :as vehicle])
  (:require [eom.constants :as cs])
  (:require [eom.atmosphere :as std])
  (:require [eom.dutycycle :as dc])
  (:require [eom.numeric :as numeric])
  (:use [eom.test-utils :only [*tolerance* approx= approx=s]])
  (:use [clojure.test :only [deftest is are testing]]))

(def veh1 (vehicle/mk-vehicle "ex1" 0.5 0.1 1.0 1.0))

(def dc1 (dc/mk-dutycycle "stopped"
                          [0.0 1.0]
                          [0.0 0.0]
                          [0.0 0.0]))

(deftest test-aero-const
  (is (approx= (std/std-air-density-kg--m3 0.0)
               (vehicle/aero-const-1--m veh1)))
  (binding [vehicle/*params* {:air-density-kg--m3 1.0}]
    (is (== 1.0 (vehicle/aero-const-1--m veh1)))))

(deftest test-rolling-const
  (is (approx= (/ cs/gravity-m--s2 10.0)
               (vehicle/rolling-const-m--s2 veh1)))
  (binding [vehicle/*params* (assoc vehicle/*params* :gravity-m--s2 10.0)]
    (is (== 1.0 (vehicle/rolling-const-m--s2 veh1)))))

(deftest test-aero-SEPDs
  (is (approx= 0.0 (first (vehicle/aero-SEPDs-m--s2 veh1 dc1)))))

(deftest test-rolling-SEPDs
  (is (approx=s [(/ cs/gravity-m--s2 10.0)]
                (vehicle/rolling-SEPDs-m--s2 veh1 dc1))))

(deftest test-SEPDs
  (is (approx=s [0.0] (vehicle/SEPDs-m--s2 veh1 dc1))))

(deftest test-SEPD
  (is (== 0.0 (vehicle/SEPD-m--s2 veh1 dc1))))

(deftest test-SEs
  (is (approx=s [0.0] (vehicle/SEs-m2--s2 veh1 dc1))))

(deftest test-SE
  (is (approx= 0.0 (vehicle/SE-m2--s2 veh1 dc1))))

(deftest test-inertial-avg-powers
  (is (= [0.0] (vehicle/inertial-avg-powers-W veh1 dc1))))

(deftest test-gravitational-potential-avg-powers-W
  (is (= [0.0]
         (vehicle/gravitational-potential-avg-powers-W
           veh1 dc1))))

(deftest test-aerodynamic-avg-powers
  (binding [*tolerance* 0.001]
    (is (approx=s [0.0]
                  (vehicle/aerodynamic-avg-powers-W veh1 dc1)))))

(deftest test-rolling-resistance-avg-powers
  (binding [*tolerance* 0.001]
    (is (approx=s [0.0]
                  (vehicle/rolling-resistance-avg-powers-W veh1 dc1)))))

(deftest test-roadload-avg-powers
  (binding [*tolerance* 0.001]
    (is (approx=s [0.0]
                  (vehicle/roadload-avg-powers-W veh1 dc1)))))

(def veh2 (vehicle/mk-vehicle "veh2" 1000.0 0.1 0.5 2.0))

(def dc2 (dc/mk-dutycycle "dc2"
                          [0.0 1.0 2.0 3.0 4.0]
                          [0.0 1.0 2.0 1.0 0.0]
                          [0.0 0.0 0.0 0.0 0.0]))

(def dc3 (dc/mk-dutycycle "dc3 -- elevator"
                          [0.0 1.0 2.0 3.0 4.0]
                          [0.0 1.0 2.0 1.0 0.0]
                          [0.0 0.5 2.0 3.5 4.0]))

(deftest test2-aero-const
  (binding [vehicle/*params* (assoc vehicle/*params* :air-density-kg--m3 2.0)]
    (= 1.0 (vehicle/aero-const-1--m veh2))))

(deftest test2-rolling-const
  (binding [vehicle/*params* (assoc vehicle/*params* :gravity-m--s2 (/ 1.0 100.0))]
    (= 1.0 (vehicle/rolling-const-m--s2 veh2))))

(deftest test2-aero-SEPD
  (testing "aero specific energy per distance"
           (let [ss (:speeds-m--s dc2)
                 ss2 (numeric/pow-ea ss 2.0)
                 ss3 (numeric/pow-ea ss 3.0)
                 aero-speeds2 (map #(/ (/ (+ %1 (* %2 %4) (* %3 %5) %6) 4.0)
                                       (/ (+ %3 %4) 2.0))
                                   (rest ss3)
                                   (rest ss2)
                                   (rest ss)
                                   ss
                                   ss2
                                   ss3)]
             (is (= aero-speeds2
                    (dc/aerodynamic-speed2s-m2--s2 dc2)))
             (binding [vehicle/*params* (assoc vehicle/*params*
                                          :air-density-kg--m3
                                          (/ 1000.0 (* 0.5 0.5 2.0)))]
               (is (= aero-speeds2
                      (vehicle/aero-SEPDs-m--s2 veh2 dc2)))))))

(deftest test2-rolling-SEPDs
  (testing "rolling specific energy per distance for a moving cycle"
           (let [g 9.81
                 rrc0 (/ g)]
             (binding [vehicle/*params* {:gravity-m--s2 g}]
               (is (= (repeat 4 (* rrc0 g))
                      (vehicle/rolling-SEPDs-m--s2
                       (assoc veh2 :rrc0 rrc0) dc2)))))))

(deftest test2-SEPDs
  (testing "specific energy per distance totals for a moving cycle"
           (let [ds [0.5 1.5 1.5 0.5]
                 spds [0.0 1.0 2.0 1.0 0.0]
                 spds2 [0.0 1.0 4.0 1.0 0.0]
                 spds3 [0.0 1.0 8.0 1.0 0.0]
                 char-accels (map #(* 0.5 (/ (- %1 %2) %3)) (rest spds2) spds2 ds)
                 aero-speeds2 (map #(/ (/ (+ %1 (* %2 %4) (* %3 %5) %6) 4.0)
                                       (/ (+ %3 %4) 2.0))
                                   (rest spds3)
                                   (rest spds2)
                                   (rest spds)
                                   spds
                                   spds2
                                   spds3)
                 M (:mass-kg veh2)
                 rho 1.225
                 CD (:CD veh2)
                 FA (:FA-m2 veh2)
                 Caero (/ (* 0.5 rho CD FA) M)
                 g cs/gravity-m--s2
                 rrc0 (:rrc0 veh2)
                 Crolling (* g rrc0)
                 SEPDs (map #(+ (* Caero %1) Crolling %2)
                            aero-speeds2 char-accels)
                 act-SEPDs (vehicle/SEPDs-m--s2 veh2 dc2)]
             (is (every? true?
                   (map approx= SEPDs act-SEPDs)))
             (is (approx= (/ (apply + (map * ds SEPDs))
                             (apply + ds))
                          (vehicle/SEPD-m--s2 veh2 dc2))))))

(deftest test2-SEs
  (testing "specific energies on transitions for a moving cycle"
           (let [ds [0.5 1.5 1.5 0.5]
                 spds [0.0 1.0 2.0 1.0 0.0]
                 spds2 [0.0 1.0 4.0 1.0 0.0]
                 spds3 [0.0 1.0 8.0 1.0 0.0]
                 char-accels (map #(* 0.5 (/ (- %1 %2) %3)) (rest spds2) spds2 ds)
                 aero-speeds2 (map #(/ (/ (+ %1 (* %2 %4) (* %3 %5) %6) 4.0)
                                       (/ (+ %3 %4) 2.0))
                                   (rest spds3)
                                   (rest spds2)
                                   (rest spds)
                                   spds
                                   spds2
                                   spds3)
                 M (:mass-kg veh2)
                 rho 1.225
                 CD (:CD veh2)
                 FA (:FA-m2 veh2)
                 Caero (/ (* 0.5 rho CD FA) M)
                 g cs/gravity-m--s2
                 rrc0 (:rrc0 veh2)
                 Crolling (* g rrc0)
                 SEPDs (map #(+ (* Caero %1) Crolling %2)
                            aero-speeds2 char-accels)
                 SEs (map * SEPDs ds)
                 act-SEs (vehicle/SEs-m2--s2 veh2 dc2)]
             (is (every? true? (map approx= SEs act-SEs)))
             (is (approx= (apply + SEs) (apply + act-SEs))))))

(deftest test2-inertial
  (testing "Testing inertial power prediction (W)"
           (let [M 1000.0
                 rho 1.225
                 FA 2.0
                 CD 0.5
                 rrc0 0.1
                 spds [0.0 1.0 2.0 1.0 0.0]
                 spds2 [0.0 1.0 4.0 1.0 0.0]
                 dspd2s [1.0 3.0 -3.0 -1.0]
                 dts [1.0 1.0 1.0 1.0]
                 IEs (map #(* 0.5 M (/ %1 %2)) dspd2s dts)]
             (is (every? true?
                         (map approx=
                              IEs
                              (vehicle/inertial-avg-powers-W veh2 dc2)))))))

(deftest test2-grav-potential
  (testing "grav potential for an 'elevator' cycle"
           (let [es [0.0 0.5 2.0 3.5 4.0]
                 dhs [0.5 1.5 1.5 0.5]
                 dds [0.5 1.5 1.5 0.5]
                 dts [1.0 1.0 1.0 1.0]
                 M 1000.0
                 rho 1.225
                 FA 2.0
                 CD 0.5
                 rrc0 0.1
                 g cs/gravity-m--s2
                 GPs (map #(/ (* g M %1) %2) dhs dts)
                 act-GPs (vehicle/gravitational-potential-avg-powers-W
                           veh2 dc3)]
             (is (every? true? (map approx= GPs act-GPs))))))

(deftest test2-aero-powers
  (testing "aerodynamic power for a moving cycle"
           (let [dts [1.0 1.0 1.0 1.0]
                 spds [0.0 1.0 2.0 1.0 0.0]
                 spds2 [0.0 1.0 4.0 1.0 0.0]
                 spds3 [0.0 1.0 8.0 1.0 0.0]
                 avg-spds3 (map #(/ (+ %1 (* %2 %4) (* %3 %5) %6)
                                    4.0)
                                (rest spds3)
                                (rest spds2)
                                (rest spds)
                                spds
                                spds2
                                spds3)
                 rho 1.225
                 FA 2.0
                 CD 0.5
                 APs (map #(* 0.5 rho CD FA %1 %2) avg-spds3 dts)
                 act-APs (vehicle/aerodynamic-avg-powers-W
                           veh2 dc2)]
             (is (every? true? (map approx= APs act-APs))))))

(deftest test2-rolling-powers
  (testing "Testing rolling avergage power on a moving cycle"
           (let [dts [1.0 1.0 1.0 1.0]
                 spds [0.0 1.0 2.0 1.0 0.0]
                 avg-spds (map #(/ (+ %1 %2) 2.0) (rest spds) spds)
                 M 1000.0
                 g cs/gravity-m--s2
                 rrc0 0.1
                 RPs (map #(* rrc0 M g %1) avg-spds)
                 act-RPs (vehicle/rolling-resistance-avg-powers-W veh2 dc2)]
             (is (every? true? (map approx= RPs act-RPs))))))

(deftest test2-roadload-powers
  (testing "Roadload powers on a moving cycle"
           (let [dts [1.0 1.0 1.0 1.0]
                 dhs [0.0 0.0 0.0 0.0]
                 spds [0.0 1.0 2.0 1.0 0.0]
                 spds2 [0.0 1.0 4.0 1.0 0.0]
                 spds3 [0.0 1.0 8.0 1.0 0.0]
                 dspd2s (map - (rest spds2) spds2)
                 avg-spds (map #(/ (+ %1 %2) 2.0) (rest spds) spds)
                 avg-spds3 (map #(/ (+ %1 (* %2 %4) (* %3 %5) %6)
                                    4.0)
                                (rest spds3)
                                (rest spds2)
                                (rest spds)
                                spds
                                spds2
                                spds3)
                 rho 1.225
                 CD 0.5
                 FA 2.0
                 M 1000.0
                 g cs/gravity-m--s2
                 rrc0 0.1
                 APs (map #(* 0.5 rho CD FA %1) avg-spds3)
                 RPs (map #(* rrc0 M g %1) avg-spds)
                 GPs (map #(/ (* g M %1) %2) dhs dts)
                 IPs (map #(/ (* 0.5 M %1) %2) dspd2s dts)
                 totals (map + APs RPs GPs IPs)
                 actuals (vehicle/roadload-avg-powers-W veh2 dc2)]
             (is (every? true? (map approx= totals actuals))))))

(deftest test3
  (let [params {:gravity-m--s2 9.81
                :air-density-kg--m3 1.2}
        veh (vehicle/mk-vehicle "veh3" 1000.0 0.01 0.35 2.5)
        dc (dc/mk-dutycycle "cbd"
                            [0.0 10.0 30.0 35.0]
                            [0.0 8.0 8.0 0.0]
                            [0.0 0.0 0.0 0.0])]
    (binding [vehicle/*params* params]
      (testing "aero-const"
        (is (approx= (vehicle/aero-const-1--m veh) 5.25e-4)))
      (testing "rolling-const"
        (is (approx= (vehicle/rolling-const-m--s2 veh) 0.0981)))
      (testing "aero-SEPDs"
        (is (approx=s [0.0168 0.0336 0.0168]
                      (vehicle/aero-SEPDs-m--s2 veh dc))))
      (testing "rolling-SEPDs"
        (is (approx=s [0.0981 0.0981 0.0981]
                      (vehicle/rolling-SEPDs-m--s2 veh dc))))
      (testing "SEPDs"
        (is (approx=s [0.9149 0.1317 -1.4851] (vehicle/SEPDs-m--s2 veh dc))))
      (testing "SEPD"
        (is (approx= 0.12711818181818 (vehicle/SEPD-m--s2 veh dc))))
      (testing "SEs"
        (is (approx=s [36.596 21.072 -29.702] (vehicle/SEs-m2--s2 veh dc))))
      (testing "SE"
        (is (approx= 27.966 (vehicle/SE-m2--s2 veh dc))))
      (testing "inertial-avg-powers"
        (is (approx=s [3200.0 0.0 -6400.0]
                      (vehicle/inertial-avg-powers-W veh dc))))
      (testing "gravitational-potential-avg-powers"
        (is (approx=s [0.0 0.0 0.0]
                      (vehicle/gravitational-potential-avg-powers-W veh dc))))
      (testing "aerodynamic-avg-powers"
        (is (approx=s [67.2 268.8 67.2]
                      (vehicle/aerodynamic-avg-powers-W veh dc))))
      (testing "rolling-resistance-avg-powers"
        (is (approx=s [392.4 784.8 392.4]
                      (vehicle/rolling-resistance-avg-powers-W veh dc))))
      (testing "roadload-avg-powers"
        (is (approx=s [3659.6 1053.6 -5940.4]
                      (vehicle/roadload-avg-powers-W veh dc))))
      (testing "inertial-energies"
        (is (approx=s [32000.0 0.0 -32000.0]
                      (vehicle/inertial-energies-J veh dc))))
      (testing "gravitational-potential-energies"
        (is (approx=s [0.0 0.0 0.0]
                      (vehicle/gravitational-potential-energies-J veh dc))))
      (testing "aerodynamic-energies"
        (is (approx=s [672.0 5376.0 336.0]
                     (vehicle/aerodynamic-energies-J veh dc))))
      (testing "rolling-energies"
        (is (approx=s [3924.0 15696.0 1962.0]
                      (vehicle/rolling-resistance-energies-J veh dc))))
      (testing "roadload-energies"
        (is (approx=s [36596.0 21072.0 -29702.0]
                      (vehicle/roadload-energies-J veh dc))))
      (testing "total-roadload"
        (is (approx= 27966.0 (vehicle/total-roadload-energy-J veh dc))))
      (testing "total-positive-roadload"
        (is (approx= 57668.0 (vehicle/total-positive-roadload-J veh dc))))
      (testing "total-negative-roadload"
        (is (approx= -29702.0 (vehicle/total-negative-roadload-J veh dc)))))))
