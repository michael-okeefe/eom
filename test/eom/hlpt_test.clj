;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.hlpt-test
  (:require [eom.dutycycle :as cyc])
  (:require [eom.vehicle :as vehicle])
  (:require [eom.hlpt :as hlpt])
  (:use [eom.test-utils :only [*tolerance* approx= approx=s]])
  (:use [clojure.test :only [deftest is are testing]]))


(def params {:gravity-m--s2 9.81
             :air-density-kg--m3 1.2})
(def veh (vehicle/mk-vehicle "veh" 1000.0 0.01 0.35 2.5))
(def dc (cyc/mk-dutycycle "cbd"
                          [0.0 10.0 30.0 35.0]
                          [0.0 8.0 8.0 0.0]
                          [0.0 0.0 0.0 0.0]))
(def fc-fn (hlpt/mk-const-eff-fc-fn 0.5 0.25))
(def fc-fn-01 (hlpt/mk-const-eff-fc-fn 0.5 0.0))
(def fc-fn-02 (hlpt/mk-const-eff-fc-fn 0.5 0.5))
(def fc-fn-03 (hlpt/mk-eff-by-power-fc-fn
               [-100000.0 -50000.0 -10000.0 0.0 10000 50000.0 100000.0]
               [-30000.0  -25000.0  -5000.0 0.0 30000 150000.0 300000.0]))

(deftest test-mk-const-eff-fc-fn
  (is (approx= 2.0 (fc-fn 1.0 1.0)))
  (is (approx= -1.0 (fc-fn -4.0 1.0))))

(deftest test-mk-eff-by-power-fc-fn
  (is (approx= 0.0 (fc-fn-03 0.0 1.0)))
  (is (approx= -5000.0 (fc-fn-03 -10000.0 1.0)))
  (is (approx= -10000.0 (fc-fn-03 -20000.0 2.0)))
  (is (approx= 60000.0 (fc-fn-03 20000.0 1.0))))

(deftest test-fuel-consumptions-inf-ess-J-01
  (testing "for 50% pt eff; 0% regen eff"
    (binding [vehicle/*params* params]
      (is (approx=s
           [(* 2 36596.0) (* 2 21072.0) 0.0]
           (hlpt/fuel-consumptions-inf-ess-J veh dc fc-fn-01))))))

(deftest test-fuel-consumptions-inf-ess-J-02
  (testing "for 50% pt eff; 50% regen eff"
    (binding [vehicle/*params* params]
      (is (approx=s
           [(* 2 36596.0) (* 2 21072.0) (* 0.50 -29702.0)]
           (hlpt/fuel-consumptions-inf-ess-J veh dc fc-fn-02))))))

(deftest test-fuel-consumptions-limited-ess-J
  (binding [vehicle/*params* params]
    (let [limits {:fc-fn fc-fn-02
                  :ess-cap-J 70000.0
                  :init-soe 1.0
                  :max-pwr-W 3000.0
                  :min-pwr-W -2000.0}]
      (is (approx=s
           [(* 2 30000.0) 10000.0 -5000.0]
           (:fcs-J
            (hlpt/fuel-consumptions-limited-ess-J veh dc limits))))
      (is (approx=s
           [70000.0 10000.0 0.0 5000.0]
           (:ess-soes-J
            (hlpt/fuel-consumptions-limited-ess-J veh dc limits)))))))
