;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.demo.back-fwd
  (:require [eom.load :as ld]
            [eom.env :as env]
            [eom.veh :as veh]
            [eom.unit-conversion :as uc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARAMETER DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def height-m (uc/length 57.9 :in)); ref [1]

(def width-m (uc/length 71.7 :in)) ; ref [1]

(def vehicle-params
  (-> {}
      (veh/with-frontal-area-m2 (* height-m width-m))
      (veh/with-drag-coefficient 0.28) ; ref [2]
      (veh/with-mass-kg (uc/mass 3190.0 :lbs-mass)) ; ref [1]
      (veh/with-rrc0 0.009) ; nominal rolling resistance
      (veh/with-top-speed-m--s (uc/speed 128.0 :mph)))) ; ref [4]

;; vehicle-params

(def environment
  (-> {}
      (env/add-constant-air-density-kg--m3-for-elevation-m
       (uc/length #_0.0 5280.0 :feet))
      (env/add-earth-normal-gravity-m--s2)))

;; (uc/length 5280.0 :feet)
;; ((:density-kg--m3-by-elevation-m environment) 0.0)
;; ((:density-kg--m3-by-elevation-m environment) (uc/length 5280.0 :feet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORWARD PATH SIMULATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def fwd-simulator (ld/mk-forward-calculator ld/typical-road-vehicle-loads
                                             vehicle-params
                                             environment))

;                           time | speed | elevation | flags | step-wise
;                            sec |  m/s  |     m     |  --   |   --
(fwd-simulator (ld/mk-state  0.0    0.0       0.0      #{}     {})
               787.4321619660382 ; Watts
               1.0               ; seconds (duration of the coming time-step)
               )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKWARD PATH SIMULATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def bwd-simulator (ld/mk-backward-calculator ld/typical-road-vehicle-loads
                                              vehicle-params
                                              environment))

;                           time | speed | elevation | flags | step-wise
;                            sec |  m/s  |     m     |  --   |   --
(bwd-simulator  (ld/mk-state 0.0    0.0       0.0      #{}     {})
                (ld/mk-state 1.0    1.0       0.0      #{}     {})
                )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REFERENCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [1] Vehicle Specifications for the 2014 Toyota Camry
;; http://www.toyota.com/camry/features.html
;; expected milage 25/35/28 for city/highway/combined
;;
;; [2] 2012 Toyota Camry Hybrid
;; http://www.caranddriver.com/reviews/2012-toyota-camry-hybrid-test-review
;;
;; [3] Goodyear Tire:
;; http://www.goodyear.com/en-US/tires/search/2014_Toyota_Camry_LE
;;
;; [4] Car and Driver: 2012 Toyota Camry Road Test Review
;; http://www.caranddriver.com/reviews/toyota-camry-se-v6-road-test-review
;; P205/65R16 -> 16" diameter
