;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.fuels
  (:require [eom.constants :as constants]
            [eom.unit-conversion :as uc]))

(defn as-kg
  "[number fuel] -> number
  Mass of fuel (kg) for the given fuel energy consumed (J) and
  fuel properties."
  [energy-J fuel]
  (/ energy-J (:HV-J--kg fuel)))

(defn as-liters
  "[number fuel] -> number
  Volume of fuel (L) for the given fuel energy consumed (J) and
  fuel properties."
  [energy-J fuel]
  (/ (as-kg energy-J fuel) (:density-kg--L fuel)))

(defn as-gallons
  "[number fuel] -> number
  Volume of fuel (gallons) for the given fuel energy consumed (J)
  and fuel properties."
  [energy-J fuel]
  (uc/convert-volume (as-liters energy-J fuel) [1.0 :L] [1.0 :gallons]))

(defn as-L--100km
  "[non-negative-number non-negative-number fuel] -> number
  Liters per 100 kilometers for the given fuel energy usage (J)
  over the given distance (m) with the given fuel's properties."
  [energy-J distance-m fuel]
  (let [distance-100km (uc/convert-length distance-m [1.0 :m] [100.0 :km])
        fuel-L (as-liters energy-J fuel)]
    (/ fuel-L distance-100km)))

(defn as-mpg
  "[number number fuel] -> number
  Miles per gallon (mpg) for the given fuel energy usage (J)
  over the given distance (m) with the given fuel's properties."
  [energy-J distance-m fuel]
  (let [distance-mi (uc/convert-length distance-m [1.0 :meter] [1.0 :mile])
        fuel-gallons (as-gallons energy-J fuel)]
    (/ distance-mi fuel-gallons)))

(defn as-gallons--100mi
  "(Fn Real Real Fuel -> Real)
  Calculate the gallons/100 miles fuel consumption"
  [energy-J distance-m fuel]
  (let [distance-100mi (uc/convert-length distance-m
                                          [1.0 :meter]
                                          [100.0 :mile])
        fuel-gallons (as-gallons energy-J fuel)]
    (/ fuel-gallons distance-100mi)))

(defn mk-fuel
  "[string number number] -> fuel
  Define a fuel based on a name (string), a heating value (J/kg),
  and a density (kg/L)."
  [name HV-J--kg density-kg--L]
  {:pre [(number? HV-J--kg) (number? density-kg--L)
         (< 0 HV-J--kg) (< 0 density-kg--L)]}
  {:name name
   :HV-J--kg HV-J--kg
   :density-kg--L density-kg--L})

(def gasoline (mk-fuel "Gasoline (LHV)" 42.6e6 0.749))

(def diesel (mk-fuel "Diesel (LHV)" 43.0e6 0.850))

(def hydrogen (mk-fuel "Hydrogen (LHV)" 120.0e6 0.018))
