;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.powerflow-test
  (:use [midje.sweet])
  (:require [clojure.test :refer :all]
            [eom.demo.back-fwd :refer [bwd-simulator]]
            [eom.powerflow :as pf])
  (:import [eom.powerflow InfSrcComp ConstEffComp LimitedOutputComp]))

(fact "bound bounds a number..."
      (fact "to be less than or equal to the upper bound"
            (pf/bound 10 8 4) => 8)
      (fact "to be greater than or equal to the lower bound"
            (pf/bound 2 8 4) => 4)
      (fact "but otherwise returns the number unchanged"
            (pf/bound 6 8 4) => 6))

(fact "split-power-first-favored loads power obligation on first in list first"
      (pf/split-power-first-favored
       {:ins [nil nil nil nil]}
       100.0
       [120.0 120.0 120. 120.0]
       [-200.0 -200.0 -200.0 -200.0]
       1.0)
      => [100.0 0.0 0.0 0.0]
      (pf/split-power-first-favored
       {:ins [nil nil nil nil]}
       100.0
       [20.0 120.0 120. 120.0]
       [-20.0 -200.0 -200.0 -200.0]
       1.0)
      => [20.0 80.0 0.0 0.0]
      (pf/split-power-first-favored
       {:ins [nil nil nil nil]}
       -100.0
       [120.0 120.0 120. 120.0]
       [-120.0 -200.0 -200.0 -200.0]
       1.0)
      => [-100.0 0.0 0.0 0.0]
      (pf/split-power-first-favored
       {:ins [nil nil nil nil]}
       -100.0
       [20.0 120.0 120. 120.0]
       [-20.0 -200.0 -200.0 -200.0]
       1.0)
      => [-20.0 -80.0 0.0 0.0])

(fact "split-power-evenly ..."
      (fact "... evenly splits power"
            (pf/split-power-evenly
             {:inputs [nil nil nil nil]}
             100.0
             [200.0 200.0 200.0 200.0]
             [0.0 0.0 0.0 0.0]
             1.0)
            => [25.0 25.0 25.0 25.0])
      (fact "re-allocating where necessary"
            (pf/split-power-evenly
             {:inputs [nil nil nil nil]}
             100.0
             [30.0 30.0 30.0 22.0]
             [0.0 0.0 0.0 0.0]
             1.0)
            => [26.0 26.0 26.0 22.0])
      (fact "... works for negative powers"
            (pf/split-power-evenly
             {:inputs [nil nil nil nil]}
             -100.0
             [30.0 30.0 30.0 30.0]
             [-30.0 -30.0 -30.0 -30.0]
             1.0)
            => [-25.0 -25.0 -25.0 -25.0])
      (fact "re-allocates for negative powers as well"
            (pf/split-power-evenly
             {:inputs [nil nil nil nil]}
             -100.0
             [30.0 30.0 30.0 30.0]
             [-30.0 -30.0 -30.0 -22.0]
             1.0)
            => [-26.0 -26.0 -26.0 -22.0]))

(fact "make-log creates a new log"
      (pf/make-log)
      => {:energy-stored-J []
          :power-out-W []
          :elapsed-time-s []
          :power-in-W []
          :power-loss-W []})

(fact "update-log updates a log by adding the given numbers to the time series"
      (pf/update-log (pf/make-log)
                     1.0 80.0 100.0 20.0 0.0)
      => {:elapsed-time-s [1.0]
          :power-in-W [100.0]
          :power-out-W [80.0]
          :power-loss-W [20.0]
          :energy-stored-J [0.0]})

(defn make-limited-src
  "(id:Keyword * max-pwr-out-W:Real * min-pwr-out-W:Real) -> Comp
  Make a power limited source component"
  [id max-pwr-out-W min-pwr-out-W]
  (pf/make-limited-output-component
   (keyword (str "limit-" (name id)))
   max-pwr-out-W
   min-pwr-out-W
   [(pf/make-infinite-source-component
     (keyword (str "source-" (name id)))
     (pf/make-log))]
   (pf/make-log)))

(fact "An InfiniteSourceComp..."
      (let [c (pf/make-infinite-source-component :fuel-tank)]
        (fact "Delivers any requested positive power"
              (pf/step c 100.0 1.0)
              => {:comp (InfSrcComp. :fuel-tank
                                     {:elapsed-time-s [1.0]
                                      :power-in-W [0.0]
                                      :power-out-W [100.0]
                                      :power-loss-W [0.0]
                                      :energy-stored-J [-100.0]})
                  :pwr-out-W 100.0})
        (fact "But cannot absorb power..."
              (pf/step c -100.0 1.0)
              => {:comp (InfSrcComp. :fuel-tank
                                     {:elapsed-time-s [1.0]
                                      :power-in-W [0.0]
                                      :power-out-W [0.0]
                                      :power-loss-W [0.0]
                                      :energy-stored-J [0.0]})
                  :pwr-out-W 0.0})
        (fact "Yields an infinite max power output capability"
              (pf/max-power-out-W c 1.0) => Double/POSITIVE_INFINITY)
        (fact "But cannot absorb any power"
              (pf/min-power-out-W c 1.0) => 0.0)))

(fact "A LimitedOutputComp..."
      (let [c (pf/make-limited-output-component
               :ice-limits
               100.0
               -100.0
               (pf/make-infinite-source-component [:fuel-tank]))]
        (fact "limits output power to its limits"
              (-> (pf/step c 120.0 1.0) :pwr-out-W) => 100.0)
        (fact "limits input power to its limits"
              (-> (pf/step c -120.0 1.0) :pwr-out-W) => 0.0)
        (fact "but allows valid powers to flow through"
              (-> (pf/step c 50.0 1.0) :pwr-out-W) => 50.0)

        (fact "has a max power limit of it's upper limit"
              (pf/max-power-out-W c 1.0) => 100.0)
        (fact "inherits a stricter min power limit from upstream"
              (pf/min-power-out-W c 1.0) => 0.0)))

(fact "A constant efficiency component..."
      (let [c (pf/make-constant-efficiency-component
               :ice
               0.5
               (pf/make-limited-output-component
                :ice-limits
                500.0
                -500.0
                (pf/make-infinite-source-component :fuel-tank)))]
        (fact "Does not itself limit output power"
              (:pwr-out-W (pf/step c 100.0 1.0)) => 100.0)
        (fact "Requires 2x the output power as input at 50% efficiency..."
              (get-in (pf/step c 100.0 1.0) [:comp :log :power-in-W])
              => [200.0])
        (fact "Has a loss of 100 W at 50% efficiency..."
              (get-in (pf/step c 100.0 1.0) [:comp :log :power-loss-W])
              => [100.0])
        (fact "Stores no energy..."
              (get-in (pf/step c 100.0 1.0) [:comp :log :energy-stored-J])
              => [0.0])
        (fact "Passes through the power limits of it's upstream components..."
              (-> (pf/step c -200.0 1.0) :pwr-out-W) => 0.0)))

(fact "Power splitter test"
      (let [f (pf/make-split-power-thermostat-controller (constantly 0.15)
                                                         0.2 100.0)]
        (f nil 120.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [100.0 20.0]
        (f nil 100.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [100.0 0.0]
        (f nil 80.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [85.0 -5.0]
        (f nil -80.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [0.0 -80.0])
      (let [f (pf/make-split-power-thermostat-controller (constantly 0.25)
                                                         0.2 100.0)]
        (f nil 120.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [100.0 20.0]
        (f nil 100.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [95.0 5.0]
        (f nil 80.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [75.0 5.0]
        (f nil -80.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [0.0 -80.0])
      (let [f (pf/make-split-power-thermostat-controller (constantly 0.55)
                                                         0.5 100000.0)]
        (f nil 120.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [100.0 20.0]
        (f nil 10.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [0.0 10.0])
      (let [f (pf/make-split-power-thermostat-controller (constantly 0.45)
                                                         0.5 100000.0)]
        (f nil 120.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [100.0 20.0]
        (f nil 10.0 [100.0 20.0] [0.0 -100.0] 1.0)
        => [100.0 -90.0]
        (f nil 10.0 [200.0 20.0] [0.0 -100.0] 1.0)
        => [110.0 -100.0]
        (f nil 10.0 [200.0 20.0] [0.0 -100.0] 1.0)
        => [110.0 -100.0])
      (let [f (pf/make-split-power-thermostat-controller (constantly 0.45)
                                                         0.5 1e10)]
        (f nil 50e3 [100e3 20e3] [0.0 -100e3] 1.0)
        => [100e3 -50e3])
      (let [f (pf/make-split-power-thermostat-controller (constantly 0.45)
                                                         0.5 1e2)]
        (f nil 50e3 [100e3 20e3] [0.0 -20e3] 1.0)
        => [(+ 50e3 5.0) -5.0]))

(fact "Powerflow/find-comp"
      (let [pt {:id :a :input {:id :b :input {:id :c :inputs [{:id :d} {:id :e}]}}}]
        (pf/find-comp pt :d)
        => {:id :d}
        (pf/find-comp pt :e)
        => {:id :e}))
