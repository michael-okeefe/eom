;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; A module to hold values pertaining to the vehicle's environment. The
;; environment must not depend on the vehicle characteristics. However, the
;; value of an environmental parameter may depend on the time-step and
;; "location" of the vehicle within the environment. For example, air density
;; depends on the vehicle's elevation but air density doesn't change with the
;; vehicle at that location.
;;
;; Metrics in the environment are typically things that we cannot control
;; such as air density, temperature, wind speed, and aspects of the road
;; surface and terrain of the course.
;;
;; Environment = f(.)
;; If it is a function of the current state but you can't control it, it
;; should be in the environment.
(ns eom.env
  (:require [eom.atmosphere :as atmos]
            [eom.constants :as const]))

(defn add-air-density-kg--m3-by-elevation-m
  "(env:Environment) -> Environment
  Adds a function of elevation (m) to air density
  under the key :density-kg--m3-by-elevation-m to
  the environment"
  [env]
  (assoc env :density-kg--m3-by-elevation-m atmos/std-air-density-kg--m3))

(defn add-constant-air-density-kg--m3-for-elevation-m
  "(env:Environment elevation-m:Number) -> Environment
  Adds a function of DutyCycleStep to air density
  that always returns the constant air density given
  by the elevation-m parameter regardless of step
  conditions."
  [env elevation-m]
  (let [density (atmos/std-air-density-kg--m3 elevation-m)]
    (assoc env :density-kg--m3-by-elevation-m (constantly density))))

(defn add-constant-air-density-kg--m3
  "(env:Environment air-density-kg--m3:Number) -> Environment
  Adds a function of DutyCycleStep to air density that
  always returns the given constant air density regardless
  of step conditions."
  [env air-density-kg--m3]
  (assoc env :density-kg--m3-by-elevation-m (constantly air-density-kg--m3)))

(defn add-earth-normal-gravity-m--s2
  "(env:Environment) -> Environment
  Adds a function that for any arguments returns gravitational
  acceleration for Earth as given in the
  eom.constants namespace."
  [env]
  (assoc env :gravity-m--s2 const/gravity-m--s2))

(defn add-constant-gravity-m--s2
  "(env:Environment * g:PositiveNumber-m--s2) -> Environment
  Adds a function that returns a constant value for gravitational
  acceleration regardless of given arguments."
  [env g]
  (assoc env :gravity-m--s2 g))

(def ^:const standard-environment
  (-> {}
      (add-air-density-kg--m3-by-elevation-m)
      (add-earth-normal-gravity-m--s2)))
