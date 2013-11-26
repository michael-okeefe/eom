;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.econ-test
  (:use [midje.sweet])
  (:require [eom.econ :as e]))

(fact "Future value given a present"
      (e/F<-P 100.0 0.01 1) => 101.0
      (e/F<-P 100.0 0.0 1) => 100.0)

(fact "Present value given a future"
      (e/P<-F 101.0 0.01 1) => 100.0
      (e/P<-F 100.0 0.0 1) => 100.0)

(fact "Future value of a recurring amount"
      (fact "is the amount at one period because A occurs at period end"
            (e/F<-A 100.0 0.01 1) => (roughly 100.0)
            (e/F<-A 100.0 0.01 2) => (roughly 201.0)))

(fact "The recurring payments equal to a future value"
      (fact "are the future value for one period"
            (e/A<-F 100.0 0.10 1) => (roughly 100.0))
      (e/A<-F 201.0 0.01 2) => (roughly 100.0))

(fact "The recurring amount equal to a present value"
      (e/A<-P 100.0 0.01 1) => (roughly 101.0)
      (e/A<-P 100.0 0 1) => 100.0)

(fact "Present value of amounts per period"
      (e/P<-A 100.0 0 1) => 100.0
      (e/P<-A 101.0 0.01 1) => (roughly 100.0))
