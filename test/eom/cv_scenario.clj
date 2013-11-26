;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.cv-scenario
  (:use [midje.sweet])
  (:require [clojure.java.io :as io]
            [eom.dutycycle :as dc]
            [eom.load :as ld]
            [eom.env :as env]
            [eom.veh :as veh]
            [eom.demo.back-fwd :as bf :only [vehicle-params]]
            [eom.powerflow :as pf]
            [eom.unit-conversion :as uc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duty Cycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def duty-cycle
  (dc/load-cycle (io/resource "dutycycles/constant_45.edn")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vehicle Loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ld/typical-road-vehicle-loads

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; env/standard-environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vehicle Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vehicle-params ; similar to a Toyota Camry

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powertrain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def pt
  (pf/make-brake-component
   :brakes
   -1e6
   (pf/make-limited-output-component
    :limits
    (uc/power 120.0 :kW)
    (uc/power 0.0 :kW)
    (pf/make-constant-efficiency-component
     :internal-combustion-engine
     0.20
     (pf/make-infinite-source-component
      :fuel-tank)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def out
  (pf/simulate ld/typical-road-vehicle-loads
               bf/vehicle-params
               env/standard-environment
               duty-cycle
               pt))

(def test-out
  (pf/simulate [(constantly 1000.0)]
               bf/vehicle-params
               env/standard-environment
               duty-cycle
               pt))


#_
(get-in (pf/find-comp (:powertrain out) :fuel-tank)
        [:log :energy-stored-J 0])
#_
(get-in (pf/find-comp (:powertrain test-out) :fuel-tank)
        [:log :energy-stored-J 0])

(fact "Fuel tank energy stored is negative"
      (let [E (get-in (pf/find-comp (:powertrain out) :fuel-tank)
                      [:log :energy-stored-J 0])
            test-E (get-in (pf/find-comp (:powertrain test-out) :fuel-tank)
                           [:log :energy-stored-J 0])]
        (< E 0.0) => true
        test-E => (roughly (* 50.0 -1000.0 (/ 0.2)))))
