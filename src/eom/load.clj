;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; The powertrain load for this vehicle
;;
;; flags --   constant over each time-step but capable of changing between
;;            time steps. Thus, instead of changing a flag during a transition,
;;            the time-step time should be shortened so the flag is constant
;;            over the step. Flags are thus assumed to change instantaneously
;;            at the sample points and are basically a step functions.
;; step-wise values -- most state values are instantaneous values at the sample
;;            time in question. However, some values may instantaneously change
;;            at a sample time and are assumed to be constant accross the time
;;            step (the changing of their values during a time step is prohibited
;;            and, in fact, changing of step-wise values is one criteria that can
;;            be used to discretize time). Step-wise values are assumed to be
;;            constant over a time-step and are held in keyword indexed maps under
;;            the :step-wise key in a vehicle state.
(ns eom.load
  (:require [clojure.set :as cset]
            [clojure.string :as string]
            [eom.numeric :as N]
            [eom.utils :refer [safe-get safe-get-in]]
            [eom.roots :as roots]))

(defn select-key-by-flag
  "(m:(Map Keyword Any) * flags:Set Keyword) -> Any
  If any of flags are keywords in m, return the corresponding
  value, otherwise, return the value of :default from map. If
  more than one keyword in flags appears in m, the behavior is
  undefined."
  [m flags]
  (reduce (fn [v flag]
            (if (contains? m flag)
              (get m flag)
              v))
          (safe-get m :default)
          flags))

(defn mk-state
  "(t:ElapsedTime-sec * spd:Speed-m--s * h:Elevation-m * flags:(Set Keyword)
    * swvs:(Map Keyword Number))
   -> State
  t :: elapsed time (from some reference) in seconds at this sample point
       t at the first sample point is usually chosen as the reference (and
       would equal 0 seconds).
  spd :: the instantaneous speed at this sample point in meters per second
  h :: elevation above some reference (usually altituted) in meters
  flags :: a set of keywords indicating time-step constant conditions the
           model may react to during the time-step transition
  swvs :: step-wise values, a map of keyword to number."
  [t spd h flags swvs]
  {:pre [(number? t) (number? spd) (number? h) (>= t 0.0) (>= spd 0.0)
         (set? flags) (map? swvs) (every? number? (vals swvs))]}
  {:time-s t
   :speed-m--s spd
   :elevation-m h
   :flags flags
   :step-wise swvs})

(defn aerodynamic-load-W
  "(x:State * x-next:State * veh:Params * env:Environment) -> Power-W
  Determine the average power over the time step (in Watts) required to meet
  aerodynamic load as a function of the current vehicle state, the next state
  (which may be either known or guessed and iterated), vehicle parameters,
  and environmental parameters"
  [x x-next veh env]
  (let [dh (- (safe-get x-next :elevation-m) (safe-get x :elevation-m))
        elevation-m (+ (* 0.5 dh) (safe-get x :elevation-m))
        flags (safe-get x :flags)
        spd-i (safe-get x :speed-m--s)
        spd-i+1 (safe-get x-next :speed-m--s)
        spd3-avg (N/binary-average3 spd-i spd-i+1)]
    (* 0.5
       ((safe-get env :density-kg--m3-by-elevation-m) elevation-m)
       (select-key-by-flag (safe-get veh :drag-coefficient) flags)
       (select-key-by-flag (safe-get veh :frontal-area-m2) flags)
       spd3-avg)))

(defn rolling-resistance0-load-W
  "(x:State * x-next:State * veh:Params * env:Environment) -> Power-W
  Determine the average power required to meet rolling resistance
  load (in Watts) for a rolling resistance model using only the
  zeroth rolling resistance coefficient (independent of speed).
  Note: assumes the same roadway conditions throughout."
  [x x-next veh env]
  (let [flags (safe-get x :flags)
        rrc0 (select-key-by-flag (safe-get veh :rrc0) flags)
        M (+ (select-key-by-flag (safe-get veh :mass-kg) flags)
             (get-in x [:step-wise :payload-kg] 0.0))
        g (safe-get env :gravity-m--s2)
        spd-i (safe-get x :speed-m--s)
        spd-i+1 (safe-get x-next :speed-m--s)
        spd-avg (N/binary-average spd-i spd-i+1)
        dt (- (safe-get x-next :time-s) (safe-get x :time-s))
        ds (* spd-avg dt)
        dh (- (safe-get x-next :elevation-m) (safe-get x :elevation-m))
        dx (Math/sqrt (- (* ds ds) (* dh dh)))
        cos-0 (if (== ds 0) 1.0 (/ dx ds))]
    (* rrc0 M g cos-0 spd-avg)))

(defn inertial-load-W
  "(x:State * x-next:State * veh:Params * env:Environment) -> Power-W
  Determine the average power required to accelerate (decelerate)
  the inertial mass of the system (in Watts)"
  [x x-next veh env]
  (let [flags (safe-get x :flags)
        M (+ (select-key-by-flag (safe-get veh :mass-kg) flags)
             (get-in x [:step-wise :payload-kg] 0.0))
        dt (- (safe-get x-next :time-s) (safe-get x :time-s))
        spd-i (safe-get x :speed-m--s)
        spd-i+1 (safe-get x-next :speed-m--s)]
    (/ (* 0.5 M (- (* spd-i+1 spd-i+1) (* spd-i spd-i)))
       dt)))

(defn gravity-load-W
  "(x:State * x-next:State * veh:Params * env:Environment) -> Power-W
  The average power required to overcome gravity for hill
  climbing (or descending) or otherwise acting with or against
  the gravitational field"
  [x x-next veh env]
  (let [flags (safe-get x :flags)
        M (+ (select-key-by-flag (safe-get veh :mass-kg) flags)
             (get-in x [:step-wise :payload-kg] 0.0))
        g (safe-get env :gravity-m--s2)
        dt (- (safe-get x-next :time-s) (safe-get x :time-s))
        dh (- (safe-get x-next :elevation-m) (safe-get x :elevation-m))]
    (/ (* M g dh)
       dt)))

(defn calc-load
  "(loads:(Seq (Fn State State Params Environment -> Power-W))
    * veh:Params * env:Environment) -> Power-W
  Reduce across the sequence of loads and return the cumulative power
  represented by the loads (in Watts)"
  [loads x x-next veh env]
  (reduce (fn [load-W f]
            (+ load-W (f x x-next veh env)))
          0.0
          loads))

(defn mk-forward-calculator
  "(loads:(Vec (Fn State State Params Environment -> Power-W))
    * veh:Params * env:Environment)
    -> (Fn State Power-W ElapsedTime-s -> State)
  Given the loads, vehicle parameters, and environmental parameters return
  a function that takes the power to the vehicle (in Watts) and
  the last vehicle state and returns the next vehicle state."
  [loads veh env]
  (let [mk-x-next (fn [x spd-i+1 dt]
                    (let [spd-i (safe-get x :speed-m--s)
                          spd-avg (/ (+ spd-i spd-i+1) 2.0)
                          dist-m (* spd-avg dt)
                          time-i (safe-get x :time-s)
                          time-i+1 (+ dt time-i)
                          incline-rad (get-in x [:step-wise :incline-rad] 0.0)
                          elev-i (safe-get x :elevation-m)
                          elev-i+1 (+ elev-i (* (Math/sin incline-rad) dist-m))]
                      {:speed-m--s spd-i+1
                       :elevation-m elev-i+1
                       :time-s time-i+1}))]
    (fn [x p dt]
      (let [f (fn [spd-i+1]
                (let [x-next (mk-x-next x spd-i+1 dt)]
                  (- p (calc-load loads x x-next veh env))))
            top-speed-m--s (get-in veh [:top-speed-m--s :default] 100.0)
            spd-i (:speed-m--s x)
            spd-i+1 (try
                      (roots/find-root f 0.0 top-speed-m--s)
                      (catch Exception e
                        (if (and (== p 0.0) (spd-i 0.0))
                          0.0
                          (roots/find-root f
                                           (max 0.0
                                                (- spd-i 4.0))
                                           (min top-speed-m--s
                                                (+ spd-i 4.0))))))]
        (mk-x-next x spd-i+1 dt)))))

(def ^:const typical-road-vehicle-loads [aerodynamic-load-W
                                         rolling-resistance0-load-W
                                         inertial-load-W
                                         gravity-load-W])

(defn mk-backward-calculator
  "(loads:(Seq (Fn State State Params Environment -> Power-W))
  * veh:Params * env:Environment)
  -> (Fn State State -> Power-W)
  Given the loads, vehicle parameters, and environmental parameters return
  a function that takes the current and next vehicle states
  and calculates the averae power over the time-step requried
  by the powertrain."
  [loads veh env]
  (fn [x x-next]
    (calc-load loads x x-next veh env)))

(defn run-cycle
  "(xs:[StatePoint] * bsim:BackwardSimulator) -> [Power-W]
  Calculate the average powers required by the powertrain"
  [xs sim]
  (let [transitions (partition 2 1 xs)]
    (map (fn [[x1 x2]] (sim x1 x2)) transitions)))
