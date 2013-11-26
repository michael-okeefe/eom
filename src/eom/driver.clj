;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; This module contains "driver models" used in forward-facing models to
;; represent the driver's control of a vehicle while trying to follow a
;; duty cycle trace.
(ns eom.driver
  (:require [eom.utils :refer [safe-get]]))

(defn mk-pi-controller [kp ki]
  "(kp:Real * ki:Real) ->
  (Fn State Target Actual ElapsedTime-s -> {:state State :control Number})
  Make a proportional-integral controller with
  constants Kp and Ki for the proportional and integral
  parts respectively."
  (fn [state target actual dt]
    (let [last-err (safe-get state :error)
          err (- target actual)
          avg-err (/ (+ err last-err) 2.0)
          integral (+ (:integral state)
                      (* ki dt avg-err))
          proportional (* kp err)
          command (+ integral proportional)]
      {:state (assoc state
                :integral integral
                :error err)
       :command command})))

(def ctrlr (mk-pi-controller 1200.0 500.0))

(defn run
  [ctrlr f init-state times targets]
  (loop [state init-state current-spd 0.0 step 0]
    (if (> step (- (count times) 2))
      state
      (let [next-time (nth times (inc step))
            this-time (nth times step)
            dt (- next-time this-time)
            this-spd-tgt (nth targets step)
            {next-state :state pwr-cmd :command}
            (ctrlr state this-spd-tgt current-spd dt)
            next-spd-act (f current-spd pwr-cmd dt)]
        (println (:error next-state))
        (recur (update-in next-state [:speeds] conj next-spd-act)
               next-spd-act
               (inc step))))))

(def f (fn [spd pwr dt]
         (if (<= pwr 0.0)
           spd
           (Math/sqrt (+ (/ (* 2.0 pwr dt) 1000.0) (* spd spd))))))

#_(run ctrlr
     f
     {:integral 0.0 :error 0.0 :speeds [0.0]}
     [0.0 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13]
     [0.0 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13]
)

