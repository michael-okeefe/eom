;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.hev-scenario
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
(def passenger-load-kg 136.0)
(def vehicle-params
  (-> {}
      (veh/with-frontal-area-m2 2.27)
      (veh/with-drag-coefficient 0.3)
      (veh/with-mass-kg (+ 1451.0 passenger-load-kg))
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

(defn make-motor
  [peak-power-W upstream-comp]
  (let [frac-outs-effs [-1.0,0.9131845841784991
                        -0.8664206642066423,0.903448275862069
                        -0.7638376383763839,0.8924949290060853
                        -0.6642066420664208,0.885192697768763
                        -0.5461254612546125,0.880324543610548
                        -0.41549815498154974,0.8791075050709942
                        -0.2649446494464945,0.883975659229209
                        -0.19483394833948334,0.8815415821501016
                        -0.16383763837638377,0.8778904665314402
                        -0.14464944649446496,0.8730223123732253
                        -0.11439114391143905,0.859634888438134
                        -0.09520295202952025,0.8438133874239353
                        -0.07011070110701098,0.8121703853955375
                        -0.05313653136531368,0.7671399594320489
                        -0.04870848708487088,0.7379310344827588
                        -0.044280442804428,0.6892494929006087
                        0,0.0
                        0.021402214022140154,0.39837728194726174
                        0.03247232472324718,0.5991886409736309
                        0.04797047970479701,0.8012170385395538
                        0.05313653136531357,0.8340770791075052
                        0.06346863468634681,0.8572008113590266
                        0.08339483394833938,0.879107505070994
                        0.1143911439114391,0.8997971602434078
                        0.1586715867158671,0.9119675456389453
                        0.21180811808118077,0.9180527383367141
                        0.27232472324723245,0.9192697768762679
                        0.35645756457564576,0.9241379310344828
                        0.4354243542435425,0.9277890466531442
                        0.5498154981549815,0.9375253549695743
                        0.6575645756457565,0.948478701825558
                        0.7660516605166052,0.9533468559837728
                        0.8752767527675278,0.9484787018255578
                        0.9416974169741699,0.9436105476673431
                        1.0,0.9496957403651116]
        [fracs effs] ((juxt #(map first %) #(map second %))
                      (partition 2 frac-outs-effs))
        outs-W (mapv #(* peak-power-W %) fracs)
        ins-W (mapv #(if (== %2 0.0)
                       0.0
                       (if (< %1 0.0)
                         (* %1 %2)
                         (/ %1 %2)))
                    outs-W
                    effs)]
    (pf/make-power-in-power-out-component
     :motor
     ins-W
     outs-W
     upstream-comp)))

(defn soe-getter
  "(Fn Comp -> Real)
  Given a component, return the state of energy of the energy
  storage system. The state of energy (SOE) is a fraction between
  0 and 1 that tells what fraction of total energy remains in the
  energy storage system"
  [c]
  (let [ess (pf/find-comp c :ess)]
    (/ (:capacity-J ess) (:max-capacity-J ess))))

(def pt
  (let [ice-branch (->> (make-ice (uc/power 78.0 :kW))
                        (pf/make-limited-output-component
                         :engine-limits
                         (uc/power 78.0 :kW)
                         (uc/power 0.0 :kW))
                        (pf/make-constant-load-component
                         :accessory-loads
                         (uc/power 3.2 :kW)))
        ess-branch (->> (let [capacity-J (* 0.37 (uc/energy 1.5 :kWh))]
                          (pf/make-energy-storage-component
                           :ess
                           capacity-J
                           (* 0.5 capacity-J)))
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
          :controller
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

(def udds-mpg (to-mpg out-udds udds))
(def hwfet-mpg (to-mpg out-hwfet hwfet))
(def comp-mpg (/ (+ (/ 0.55 udds-mpg) (/ 0.45 hwfet-mpg))))

(fact "Fuel economy for Hybrid should be about 31.4 mpg"
      comp-mpg => (roughly 31.4 1.5))
