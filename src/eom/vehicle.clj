;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.vehicle
  (:require [eom.numeric :as numeric]
            [eom.constants :as const]
            [eom.dutycycle :as cyc]
            [eom.numeric :as numeric]
            [eom.atmosphere :as atmos]
            [eom.resources :as rsrc])
  (:refer-clojure :rename {name core-name}))

(def example-vehicle
  {:name "Example Vehicle"
   :mass-kg 1000.0
   :rrc0 0.01
   :CD 0.35
   :FA-m2 2.5})

(def ^:dynamic *params*
  {:gravity-m--s2 const/gravity-m--s2
   :air-density-kg--m3 (atmos/std-air-density-kg--m3 0.0)})

(defn aero-const-1--m
  "[veh] -> float where
  Returns the constant of aerodynamics for this vehicle (1/m)

  * Parameters: vehicle/*params*
    :air-density-kg--m3"
  [veh]
  (/ (* 0.5 (:air-density-kg--m3 *params*)
        (:CD veh) (:FA-m2 veh)) (:mass-kg veh)))

(defn rolling-const-m--s2
  "[veh] -> float where
  Returns the vehicle constant for rolling resitance (m/s2).

  * Parameters: vehicle/*params*
    :gravity-m--s2"
  [veh]
  (* (:rrc0 veh) (:gravity-m--s2 *params*)))

(defn aero-SEPDs-m--s2
  "[veh dc] -> seq float where
  Returns sequence of specific energy per unit distance required
  to overcome aerodynamic drag for each transition (m/s2). A
  transition is the event from one sample point to the next."
  [veh dc]
  (let [spds2 (cyc/aerodynamic-speed2s-m2--s2 dc)
        Caero (aero-const-1--m veh)]
     (numeric/mul-ea (repeat Caero) spds2)))

(defn rolling-SEPDs-m--s2
  "[veh dc] -> seq float where
  Returns sequence of specific energy per unit distance required
  to overcome rolling resistance (m/s2)"
  [veh dc]
  (repeat (cyc/num-transitions dc) (rolling-const-m--s2 veh)))

(defn SEPDs-m--s2
  "[veh dc] -> seq float where
  Returns sequence of net specific energy per unit distance over each
  time-step (m/s2)."
  [veh dc]
  (let [aero-spds2-m2--s2 (cyc/aerodynamic-speed2s-m2--s2 dc)
        char-accels-m--s2 (cyc/characteristic-accelerations-m--s2 dc)
        Caero (aero-const-1--m veh)
        Croll (rolling-const-m--s2 veh)]
    (numeric/add-ea
     (map #(* Caero %) aero-spds2-m2--s2)
     (map #(if (> % 0.0) Croll 0.0) aero-spds2-m2--s2)
     char-accels-m--s2)))

(defn SEPD-m--s2
  "[veh dc] -> float where
  Returns the cycle total specific energy per distance (m/s2)"
  [veh dc]
  (let [Ds (cyc/distance-diffs-m dc)
        D (numeric/sum Ds)
        SEPDs (SEPDs-m--s2 veh dc)]
    (numeric/safe-div 0.0
                      (numeric/sum (numeric/mul-ea SEPDs Ds))
                      D)))

(defn SEs-m2--s2
  "[veh dc] -> seq float where
  Returns the specific energies for all transitions (i.e. time steps) of the
  duty cycle (m2/s2)"
  [veh dc]
  (let [Ds (cyc/distance-diffs-m dc)
        SEPDs (SEPDs-m--s2 veh dc)]
    (numeric/mul-ea SEPDs Ds)))

(defn SE-m2--s2
  "[veh dc] -> float
  Return the specific energy required by the vehicle driving over the given
  dutycycle (m2/s2)"
  [veh dc]
  (numeric/sum (SEs-m2--s2 veh dc)))

(defn inertial-avg-powers-W
  "[veh dc] -> seq float
  Return the average power (W) required per time-step to overcome
  inertial forces (i.e., change momentum)."
  [veh dc]
  (map #(* 0.5 (:mass-kg veh) %1)
       (numeric/safe-div-ea 0.0
                            (cyc/speeds2-difference-m2--s2 dc)
                            (cyc/time-steps-s dc))))

(defn gravitational-potential-avg-powers-W
  "[veh dc] -> seq float
  Return the average power (W) required per time-step to overcome
  gravitational potential forces (i.e., hill climbing).

  * Parameters: *params*
    :gravity-m--s2"
  [veh dc]
  (map (partial * (:mass-kg veh) (:gravity-m--s2 *params*))
       (cyc/climbing-rates-m--s dc)))

(defn aerodynamic-avg-powers-W
  "[veh dc] -> seq float
  Return the average power (W) required per time-step to overcome aerodynamic
  drag forces.

  * Parameters: *params*
    :air-density-kg--m3"
  [veh dc]
  (let [Caero (* 0.5 (:air-density-kg--m3 *params*) (:CD veh) (:FA-m2 veh))]
    (map (partial * Caero) (cyc/average-speeds-cubed-m3--s3 dc))))

(defn rolling-resistance-avg-powers-W
  "[veh dc] -> seq float
  Return the average power (W) required per time-step to overcome
  rolling resistance forces."
  [veh dc]
  (map (partial * (:mass-kg veh) (:rrc0 veh) (:gravity-m--s2 *params*))
       (cyc/average-speeds-m--s dc)))

(defn roadload-avg-powers-W
  "[veh dc] -> seq float
  Return the roadload average power (W) per each time-step. Road load
  is the sum of the power required to overcome inertial, gravitational
  potential, aerodynamic, and rolling resistance forces."
  [veh dc]
  (numeric/add-ea (inertial-avg-powers-W veh dc)
                  (gravitational-potential-avg-powers-W veh dc)
                  (aerodynamic-avg-powers-W veh dc)
                  (rolling-resistance-avg-powers-W veh dc)))

(defn- build-energy-func
  [f]
  (fn [veh dc]
    (let [dts (cyc/time-steps-s dc)]
      (map * (f veh dc) dts))))

(def
  ^{:doc "[veh dc] -> seq float
         Return inertial energies each transition (J)."}
  inertial-energies-J (build-energy-func inertial-avg-powers-W))

(def
  ^{:doc "[veh dc] -> seq float
         Gravitational potential energies each transition (J)."}
  gravitational-potential-energies-J
  (build-energy-func gravitational-potential-avg-powers-W))

(def
  ^{:doc "[veh dc] -> seq float
         Energies to overcome aerodynamic drag each transition (J)."}
  aerodynamic-energies-J
  (build-energy-func aerodynamic-avg-powers-W))

(def
  ^{:doc "[veh dc] -> seq float
         Rolling resistance energies per time step (J)."}
  rolling-resistance-energies-J
  (build-energy-func rolling-resistance-avg-powers-W))

(defn roadload-energies-J
  "[veh dc] -> seq float
  The energies to overcome roadload each transition (J)."
  [veh dc]
  (let [dts (cyc/time-steps-s dc)
        RLs (roadload-avg-powers-W veh dc)]
    (map * RLs dts)))

(defn total-roadload-energy-J
  "[veh dc] -> float
  The total roadload energy over the cycle (J)"
  [veh dc]
  (numeric/sum (roadload-energies-J veh dc)))

(defn total-positive-roadload-J
  "[veh dc] -> float
  The total positive roadload (i.e., energy required to
  overcome roadload forces) (J)"
  [veh dc]
  (numeric/sum (map numeric/positive (roadload-energies-J veh dc))))

(defn total-negative-roadload-J
  "[veh dc] -> float
  The total negative roadload (i.e., energy available from
  roadload forces -- usually due to deceleration and/or hill
  descent) (J)"
  [veh dc]
  (numeric/sum (map numeric/negative (roadload-energies-J veh dc))))

(defn mk-vehicle
  "[str (kg)float (N/N)float float float] -> veh
  Create a vehicle with a given name (string), mass in kg (0 inf),
  rolling resistance coefficient in N/N [0 inf), coefficient
  of drag [0 1), and frontal area in m2 (0 inf)"
  [name mass-kg rrc0 CD FA-m2]
  {:pre [(number? mass-kg) (> mass-kg 0) (number? rrc0) (>= rrc0 0)
         (number? CD) (>= CD 0) (number? FA-m2) (> FA-m2 0)]}
  {:name name :mass-kg mass-kg :rrc0 rrc0 :CD CD :FA-m2 FA-m2})

(defn load-vehicle
  "path -> vehicle
  Load a vehicle from the given path and return it."
  [path-to-vehicle]
  (rsrc/load-edn path-to-vehicle
                 {'eom/vehicle
                  (fn [{:keys [name mass-kg rrc0 CD FA-m2]}]
                    (mk-vehicle name mass-kg rrc0 CD FA-m2))}))
