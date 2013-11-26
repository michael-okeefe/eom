;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.atmosphere
  (:require [eom.constants :as const]))

(def
  ^{:ref-url "http://en.wikipedia.org/wiki/Density_of_air"}
  molar-mass-of-dry-air-kg--mol 0.0289644)

(defn std-temperature-K
  "[number] -> number
  Calculate the standard temperature for the given elevation
  above sea level (m)"
  [elevation-m]
  (let [h elevation-m
        T0 const/sea-level-standard-temperature-K
        L const/temperature-lapse-rate-K--m]
    (- T0 (* L h))))

(defn std-pressure-kPa
  "[number] -> number
  The standard atmospheric pressure (kPa) for the given elevation
  above sea level (m)"
  [elevation-m]
  (let [h elevation-m
        L const/temperature-lapse-rate-K--m
        T0 const/sea-level-standard-temperature-K
        p0 const/sea-level-standard-atmospheric-pressure-kPa
        M molar-mass-of-dry-air-kg--mol
        R const/ideal-gas-constant-J--mol-K
        g const/gravity-m--s2]
    (* p0 (Math/pow (- 1.0 (/ (* L h) T0))
                    (/ (* g M)
                       (* R L))))))

(defn std-air-density-kg--m3
  "[number] -> number
  The standard air density (kg/m3) at the given elevation
  above sea level (m)"
  [elevation-m]
  (let [h elevation-m
        R const/ideal-gas-constant-J--mol-K
        M molar-mass-of-dry-air-kg--mol
        T (std-temperature-K h)
        ; note: convert pressure to Pa
        p (* 1000.0 (std-pressure-kPa h))]
    (/ (* p M)
       (* R T))))
