;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.cv-scenario2
  (:use [midje.sweet])
  (:require [clojure.java.io :as io]
            [eom.dutycycle :as dc]
            [eom.load :as ld]
            [eom.env :as env]
            [eom.veh :as veh]
            [eom.dutycycle :as dc]
            [eom.powerflow :as pf]
            [eom.fuels :as fuels]
            [eom.unit-conversion :as uc]))

;; Based on
;; Simpson, A. (2006). "Cost-Benefit Analysis of Plug-in
;; Hybrid Electric Vehicle Technology". NREL/CP-540-40485.
;; http://www.nrel.gov/vehiclesandfuels/vsa/pdfs/40485.pdf
;; See also:
;; http://www.nrel.gov/vehiclesandfuels/vsa/pdfs/40969.pdf
;; http://www.nrel.gov/vehiclesandfuels/energystorage/pdfs/40847.pdf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duty Cycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def udds
  (dc/load-cycle (io/resource "dutycycles/udds.edn")))
(def hwfet
  (dc/load-cycle (io/resource "dutycycles/hwfet.edn")))

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
(def vehicle-params
  (-> {}
      (veh/with-frontal-area-m2 2.27)
      (veh/with-drag-coefficient 0.3)
      (veh/with-mass-kg 1565.0)
      (veh/with-rrc0 0.009)
      (veh/with-top-speed-m--s (uc/speed 110.0 :miles-per-hour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powertrain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-ice
  [peak-power-W]
  (let [frac-outs-effs [0.000,0.000
                        0.036,0.144
                        0.055,0.200
                        0.073,0.226
                        0.117,0.260
                        0.200,0.300
                        0.280,0.318
                        0.375,0.331
                        0.482,0.338
                        0.591,0.336
                        0.719,0.325
                        0.846,0.310
                        1.000,0.284]
        [fracs effs] ((juxt #(map first %) #(map second %))
                      (partition 2 frac-outs-effs))
        outs-W (mapv #(* peak-power-W %) fracs)
        ins-W (mapv #(if (== %2 0.0) 0.0 (/ %1 %2)) outs-W effs)]
    (pf/make-power-in-power-out-component
     :ice
     ins-W
     outs-W
     (pf/make-infinite-source-component :fuel-tank))))

(def pt
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def out-udds
  (pf/simulate ld/typical-road-vehicle-loads
               vehicle-params
               env/standard-environment
               udds
               pt))

(def out-hwfet
  (pf/simulate ld/typical-road-vehicle-loads
               vehicle-params
               env/standard-environment
               hwfet
               pt))

(defn to-L--100km
  [out dutycycle]
  (fuels/as-L--100km (- (reduce +
                                0.0
                                (get-in (pf/find-comp (:powertrain out)
                                                      :fuel-tank)
                                        [:log :energy-stored-J])))
                     (dc/distance-m dutycycle)
                     fuels/gasoline))

(defn to-mpg
  [out dutycycle]
  (fuels/as-mpg (- (reduce +
                           0.0
                           (get-in (pf/find-comp (:powertrain out)
                                                 :fuel-tank)
                                   [:log :energy-stored-J])))
                (dc/distance-m dutycycle)
                fuels/gasoline))

(def udds-L--100km (to-L--100km out-udds udds))
(def hwfet-L--100km (to-L--100km out-hwfet hwfet))

(fact "Fuel economy for UDDS is roughly 10.6 L/100km (22.2 mpg)"
      udds-L--100km => (roughly 10.6 0.05))

(fact "Fuel economy for HWFET is roughly 6.7 L/100km ( mpg)"
      hwfet-L--100km => (roughly 6.7 0.05))
