;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; A module to hold values pertaining to vehicle characteristics. Vehicle
;; characteristics must not depend on the environment it's in. However, the
;; value of a vehicle characteristic can change depending on the time-step
;; "signal" (e.g., CD could change if a "window down" signal was given).
;;
;; Vehicle characteristics are parameters in the sense that they are either
;; constant over a given run or enumerated by a finite number of potential
;; cases over a run (the cases being determined by time-step "signals").
;;
;; Vehicle characteristics are modeled as a map with the value being a map
;; with one key of :default which is used for the default case. To let the
;; default case change with time-step signals, a key corresponding to an
;; active signal must be present. As a specific example, the vehicle
;; characteristics for the above drag coefficient that changes when the
;; windows are down would be represented as:
;;
;; {:drag-coefficient {:default     0.2
;;                     :window-down 0.3}}
;;
;; Vehicle metrics are typically constant over a run. Examples are curb mass,
;; drag coefficent, etc. Vehicle characteristics can only be a function of the
;; time-step "state" and the environment. For example, rolling resistance could
;; be modeled as a function of environmental terrain (paved road vs dirt road).
;;
;; Vehicle = f(step)
;; If it is a measurable parameter of the vehicle at different states, it's in
;; vehicle.
(ns eom.veh)

(defn add-item-for-case
  [m key val case]
  (if (contains? m key)
    (assoc-in m [key case] val)
    (assoc m key {case val})))

(defn with-frontal-area-m2
  "(veh:Vehicle * FA-m2:PositiveNumber) -> Vehicle
  |(veh:Vehicle * FA-m2:PositiveNumber * case:Keyword) -> Vehicle
  Adds frontal area (m2) of a vehicle under the key :frontal-area-m2
  with a case of :default if not provided or per the provided case
  keyword if given"
  ([veh FA-m2]
   (with-frontal-area-m2 veh FA-m2 :default))
  ([veh FA-m2 case]
   {:pre [(> FA-m2 0.0)]}
   (add-item-for-case veh :frontal-area-m2 FA-m2 case)))

(defn with-drag-coefficient
  "(veh:Vehicle * CD:PositiveFraction) -> Vehicle
  |(veh:Vehicle * CD:PositiveFraction * case:Keyword) -> Vehicle
  Adds drag coefficient under the keyword :drag-coefficient with
  a case of :default if not provided or per the provided case keyword
  if given"
  ([veh CD]
   (with-drag-coefficient veh CD :default))
  ([veh CD case]
   {:pre [(>= CD 0.0) (<= CD 1.0)]}
   (add-item-for-case veh :drag-coefficient CD case)))

(defn with-mass-kg
  "(veh:Vehicle * mass-kg:PositiveNumber) -> Vehicle
  |(veh:Vehicle * mass-kg:PositiveNumber * case:Keyword) -> Vehicle
  Adds vehicle mass (kg) under the keyword :mass-kg with a case of
  :default if not provided or per the provided case keyword if given"
  ([veh mass-kg]
   (with-mass-kg veh mass-kg :default))
  ([veh mass-kg case]
   {:pre [(number? mass-kg) (> mass-kg 0.0)]}
   (add-item-for-case veh :mass-kg mass-kg case)))

(defn with-rrc0
  "(veh:Vehicle rrc0:PositiveNumber) -> Vehicle
  Adds zeroth rolling resistance coefficient (rolling resistance
  independent of speed) under the keyword :rolling-resistance-0 with
  a case of :default if not provided or per the provided case keyword
  if given"
  ([veh rrc0]
   (with-rrc0 veh rrc0 :default))
  ([veh rrc0 case]
   {:pre [(number? rrc0) (> rrc0 0.0)]}
   (add-item-for-case veh :rrc0 rrc0 case)))

(defn with-top-speed-m--s
  "(veh:Vehicle * top-speed:Speed-m--s) -> Vehicle
  Adds a top speed to the vehicle parameters"
  ([veh top-speed]
   (with-top-speed-m--s veh top-speed :default))
  ([veh top-speed case]
   {:pre [(number? top-speed) (>= top-speed 0.0)]}
   (add-item-for-case veh :top-speed-m--s top-speed case)))
