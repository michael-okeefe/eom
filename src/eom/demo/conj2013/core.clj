;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.demo.conj2013.core
  (:require [clojure.java.io :as io]
            [eom.dutycycle :as dc]
            [eom.load :as ld]
            [eom.env :as env]
            [eom.veh :as veh]
            [eom.dutycycle :as dc]
            [eom.powerflow :as pf]
            [eom.fuels :as fuels]
            [eom.demo.conj2013.helpers
             :refer [make-ice make-ice-atkins make-motor
                     soe-getter to-mpg to-L--100km
                     mpg-composite]]
            [eom.unit-conversion :as uc]))

;; Based on
;; Simpson, A. (2006). "Cost-Benefit Analysis of Plug-in
;; Hybrid Electric Vehicle Technology". NREL/CP-540-40485.
;; http://www.nrel.gov/vehiclesandfuels/vsa/pdfs/40485.pdf
;; See also:
;; http://www.nrel.gov/vehiclesandfuels/vsa/pdfs/40969.pdf
;; http://www.nrel.gov/vehiclesandfuels/energystorage/pdfs/40847.pdf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vehicle Loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ld/typical-road-vehicle-loads

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duty Cycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def city
  (dc/load-cycle (io/resource "dutycycles/udds.edn")))
(def highway
  (dc/load-cycle (io/resource "dutycycles/hwfet.edn")))
city
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vehicle Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def passenger-load-kg 136.0)
(def conventional-vehicle-params
  (-> {}
      (veh/with-frontal-area-m2 2.27)
      (veh/with-drag-coefficient 0.3)
      (veh/with-mass-kg (+ 1429.0 passenger-load-kg))
      (veh/with-rrc0 0.009)
      (veh/with-top-speed-m--s (uc/speed 110.0 :miles-per-hour))))

conventional-vehicle-params
#_
(uc/convert-speed
 (get-in conventional-vehicle-params [:top-speed-m--s :default])
 [1 :meters-per-second] [1 :miles-per-hour])
#_
(veh/with-mass-kg conventional-vehicle-params
                  1650.0
                  :with-trailer
                  )

(def hybrid-electric-vehicle-params
  (-> {}
      (veh/with-frontal-area-m2 2.23) ;; 2.23 for Prius, 2.27 for Ref 1 vehicle
      (veh/with-drag-coefficient 0.25) ;; 0.25 for Prius, 0.30 for Ref 1 vehicle
      (veh/with-mass-kg (+ 1380.0 passenger-load-kg)) ;; 1380.0 for Prius, 1451.0 for Ref 1 vehicle
      (veh/with-rrc0 0.009)
      (veh/with-top-speed-m--s (uc/speed 110.0 :miles-per-hour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

env/standard-environment
#_
(:gravity-m--s2 env/standard-environment)

(:density-kg--m3-by-elevation-m env/standard-environment)

((:density-kg--m3-by-elevation-m env/standard-environment) 0.0)

((:density-kg--m3-by-elevation-m env/standard-environment)
 (uc/convert-length 1.0 [1 :mile] [1 :meter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powertrain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def conventional-vehicle-powertrain
  (->> (make-ice (uc/power 122.0 :kW))
       (pf/make-limited-output-component
        :engine-limits
        (uc/power 122.0 :kW)
        (uc/power 0.0 :kW))
       (pf/make-constant-load-component
        :accessory-loads
        (uc/power 3.2 :kW))
       (pf/make-constant-efficiency-component
        :driveline
        0.98)
       (pf/make-brake-component
        :brakes
        -1e6)))

(def hybrid-electric-vehicle-powertrain
  (let [ice-branch (->> #_(make-ice (uc/power 78.0 :kW)) ; use this engine for reference vehicle
                        (make-ice-atkins (uc/power 73.0 :kW)) ; this engine for Prius,
                        (pf/make-limited-output-component
                         :engine-limits
                         (uc/power 78.0 :kW)
                         (uc/power 0.0 :kW))
                        (pf/make-constant-load-component
                         :accessory-loads
                         (uc/power 1.6 :kW))) ; 1.6 for Prius (estimate), 3.2 for reference vehicle
        ess-branch (->> (let [capacity-J (* 0.37 (uc/energy 1.5 :kWh))]
                          (pf/make-energy-storage-component
                           :ess
                           capacity-J
                           (* 0.5782 capacity-J)))
                        (pf/make-constant-efficiency-component
                         :ess-efficiency
                         0.90)
                        (pf/make-limited-output-component
                         :ess-limits
                         (uc/power (* 33.4 1.5) :kW)
                         (uc/power (* -33.4 1.5) :kW))
                        (pf/make-constant-load-component
                         :elec-accessory-loads
                         (uc/power 0.8 :kW))
                        (make-motor (uc/power 38.0 :kW))
                        (pf/make-limited-output-component
                         :motor-limits
                         (uc/power 38.0 :kW)
                         (uc/power -38.0 :kW)))]
    (->> (pf/make-power-coupling
          :planetary-gear
          (pf/make-split-power-thermostat-controller soe-getter
                                                     0.5
                                                     (* 0.37 (uc/energy 1.5 :kWh)))
          [ice-branch ess-branch])
         (pf/make-constant-efficiency-component
          :driveline
          0.98)
         (pf/make-brake-component
          :brakes
          -1e6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cv-out-city
  (pf/simulate ld/typical-road-vehicle-loads
               conventional-vehicle-params
               env/standard-environment
               city
               conventional-vehicle-powertrain))

(def cv-out-highway
  (pf/simulate ld/typical-road-vehicle-loads
               conventional-vehicle-params
               env/standard-environment
               highway
               conventional-vehicle-powertrain))

(def hev-out-city
  (pf/simulate ld/typical-road-vehicle-loads
               hybrid-electric-vehicle-params
               env/standard-environment
               city
               hybrid-electric-vehicle-powertrain))

(def hev-out-highway
  (pf/simulate ld/typical-road-vehicle-loads
               hybrid-electric-vehicle-params
               env/standard-environment
               highway
               hybrid-electric-vehicle-powertrain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cv-city-mpg (to-mpg cv-out-city city))
(def cv-highway-mpg (to-mpg cv-out-highway highway))
(def cv-comp-mpg (mpg-composite cv-city-mpg cv-highway-mpg))
(def hev-city-mpg (to-mpg hev-out-city city))
(def hev-highway-mpg (to-mpg hev-out-highway highway))
(def hev-comp-mpg (mpg-composite hev-city-mpg hev-highway-mpg))

; check the initial state of charge
(/ (:capacity-J (pf/find-comp (:powertrain hev-out-highway) :ess))
   (:max-capacity-J (pf/find-comp (:powertrain hev-out-highway) :ess)))

cv-city-mpg
cv-highway-mpg
cv-comp-mpg
; expected 26.7 mpg

hev-city-mpg
hev-highway-mpg
hev-comp-mpg
; expected 31.4
