;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.load-test
  (:use [midje.sweet])
  (:require [eom.load :as ld]
            [eom.veh :as veh]
            [eom.env :as env]))

(fact "Select key by signal selects :default when no match, otherwise the case
      corresponding to the signal"
      (ld/select-key-by-flag {:default 0.2 :windows-down 0.3} #{:windows-down})
      => 0.3
      (ld/select-key-by-flag {:default 0.2 :windows-down 0.3} #{:windows-up})
      => 0.2
      (ld/select-key-by-flag {:default 0.2 :windows-down 0.3} #{})
      => 0.2)

(fact "Aerodynamic load is 1/2 rho CD FA v3"
      (ld/aerodynamic-load-W (ld/mk-state 0.0 0.0 0.0 #{} {})
                             (ld/mk-state 1.0 0.0 0.0 #{} {})
                             {:drag-coefficient {:default 0.5}
                              :frontal-area-m2 {:default 1.0}}
                             {:density-kg--m3-by-elevation-m (constantly 1.0)})
      => 0.0
      (ld/aerodynamic-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                             (ld/mk-state 1.0 10.0 0.0 #{} {})
                             {:drag-coefficient {:default 0.5}
                              :frontal-area-m2 {:default 1.0}}
                             {:density-kg--m3-by-elevation-m (constantly 1.0)})
      => (* 0.5 1.0 1.0 0.5 10.0 10.0 10.0)
      (ld/aerodynamic-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                             (ld/mk-state 1.0 10.0 10.0 #{} {})
                             {:drag-coefficient {:default 0.5}
                              :frontal-area-m2 {:default 1.0}}
                             {:density-kg--m3-by-elevation-m (constantly 1.0)})
      => (* 0.5 1.0 1.0 0.5 10.0 10.0 10.0)
      (ld/aerodynamic-load-W (ld/mk-state 0.0 5.0 0.0 #{} {})
                             (ld/mk-state 10.0 10.0 0.0 #{} {})
                             {:drag-coefficient {:default 0.5}
                              :frontal-area-m2 {:default 1.0}}
                             {:density-kg--m3-by-elevation-m (constantly 1.0)})
      => (* 0.5 1.0 1.0 0.5
            (/ (+ (* 5.0 5.0 5.0)
                  (* 5.0 5.0 10.0)
                  (* 5.0 10.0 10.0)
                  (* 10.0 10.0 10.0))
               4.0)))

(fact "Rolling resistance model = RRC0 M g v-avg"
      (ld/rolling-resistance0-load-W (ld/mk-state 0.0 0.0 0.0 #{} {})
                                     (ld/mk-state 1.0 0.0 0.0 #{} {})
                                     {:mass-kg {:default 1000.0}
                                      :rrc0 {:default 0.01}}
                                     {:gravity-m--s2 10.0})
      => 0.0
      (ld/rolling-resistance0-load-W (ld/mk-state 0.0 8.0 0.0 #{} {})
                                     (ld/mk-state 1.0 12.0 0.0 #{} {})
                                     {:mass-kg {:default 1000.0}
                                      :rrc0 {:default 0.01}}
                                     {:gravity-m--s2 10.0})
      => (* 0.01 1000.0 10.0 10.0)
      (ld/rolling-resistance0-load-W (ld/mk-state 0.0 8.0 0.0 #{}
                                                          {:payload-kg 100.0})
                                     (ld/mk-state 1.0 12.0 0.0 #{} {})
                                     {:mass-kg {:default 900.0}
                                      :rrc0 {:default 0.01}}
                                     {:gravity-m--s2 10.0})
      => (* 0.01 1000.0 10.0 10.0)
      (ld/rolling-resistance0-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                                     (ld/mk-state 1.0 10.0 1.0 #{} {})
                                     {:mass-kg {:default 1000.0}
                                      :rrc0 {:default 0.01}}
                                     {:gravity-m--s2 10.0})
      => (roughly
          (* 0.01 1000.0 10.0 (Math/cos (Math/asin (/ 1.0 10.0))) 10.0))
      (ld/rolling-resistance0-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                                     (ld/mk-state 1.0 10.0 10.0 #{} {})
                                     {:mass-kg {:default 1000.0}
                                      :rrc0 {:default 0.01}}
                                     {:gravity-m--s2 10.0})
      => 0.0)

(fact "Inertial load is 1/2 M dv2 / dt"
      (ld/inertial-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                          (ld/mk-state 2.0 10.0 0.0 #{} {})
                          {:mass-kg {:default 1000.0}}
                          {})
      => 0.0
      (ld/inertial-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                          (ld/mk-state 2.0 12.0 0.0 #{} {})
                          {:mass-kg {:default 1000.0}}
                          {})
      => (* 0.5 1000.0 (- 144.0 100.0) 0.5))

(fact "Gravitational potential load is M * g * h / dt"
      (ld/gravity-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                         (ld/mk-state 2.0 10.0 0.0 #{} {})
                         {:mass-kg {:default 1000.0}}
                         {:gravity-m--s2 10.0})
      => 0.0
      (ld/gravity-load-W (ld/mk-state 0.0 10.0 0.0 #{} {})
                         (ld/mk-state 2.0 10.0 1.0 #{} {})
                         {:mass-kg {:default 1000.0}}
                         {:gravity-m--s2 10.0})
      => (* 1000.0 10.0 1.0 0.5))

(fact "calc-load gives the sum of all loads"
      (ld/calc-load [(constantly 10.0)
                     (constantly 10.0)
                     (constantly 10.0)
                     (constantly 10.0)]
                    {}
                    {}
                    {}
                    {})
      => 40.0
      (ld/calc-load [ld/aerodynamic-load-W
                     ld/rolling-resistance0-load-W
                     ld/inertial-load-W
                     ld/gravity-load-W]
                    (ld/mk-state 0.0 8.0 0.0 #{} {})
                    (ld/mk-state 2.0 12.0 1.0 #{} {})
                    {:mass-kg {:default 1000.0}
                     :drag-coefficient {:default 0.5}
                     :rrc0 {:default 0.01}
                     :frontal-area-m2 {:default 1.0}}
                    {:density-kg--m3-by-elevation-m (constantly 1.0)
                     :gravity-m--s2 10.0})
      => (+ (* 0.5 1.0 1.0 0.5 (/ (+ (* 8.0 8.0 8.0)
                                     (* 8.0 8.0 12.0)
                                     (* 8.0 12.0 12.0)
                                     (* 12.0 12.0 12.0))
                                  4.0))
            (* 0.01 1000.0 10.0 (Math/cos (Math/asin (/ 1.0 20.0))) 10.0)
            (* 0.5 1000.0 (- 144.0 64.0) 0.5)
            (* 1000.0 10.0 1.0 0.5)))

(fact "mk-forward-calculator")

(fact "mk-backward-calculator"
      (let [bc (ld/mk-backward-calculator [ld/aerodynamic-load-W
                                           ld/rolling-resistance0-load-W
                                           ld/inertial-load-W
                                           ld/gravity-load-W]
                                          {:mass-kg {:default 1000.0}
                                           :drag-coefficient {:default 0.5}
                                           :rrc0 {:default 0.01}
                                           :frontal-area-m2 {:default 1.0}}
                                          {:density-kg--m3-by-elevation-m
                                           (constantly 1.0)
                                           :gravity-m--s2 10.0})]
        (bc (ld/mk-state 0.0 8.0 0.0 #{} {})
            (ld/mk-state 2.0 12.0 1.0 #{} {}))
        => (+ (* 0.5 1.0 1.0 0.5 (/ (+ (* 8.0 8.0 8.0)
                                       (* 8.0 8.0 12.0)
                                       (* 8.0 12.0 12.0)
                                       (* 12.0 12.0 12.0))
                                    4.0))
              (* 0.01 1000.0 10.0 (Math/cos (Math/asin (/ 1.0 20.0))) 10.0)
              (* 0.5 1000.0 (- 144.0 64.0) 0.5)
              (* 1000.0 10.0 1.0 0.5))))

