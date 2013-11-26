;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.fuels-test
  (:use midje.sweet)
  (:require [eom.fuels :as fuels]
            [eom.test-utils :refer [*tolerance* approx= approx=s]]
            [clojure.test :refer [deftest is are testing]]))

(fact "Facts about Gasoline"
      (let [gas fuels/gasoline
            D-m 220.0
            E-J 1000.0]
        (approx= (fuels/as-kg E-J gas) 23.47417840375586e-6)
        => true
        (approx= (fuels/as-liters E-J gas) 31.34069212784495e-6)
        => true
        (approx= (fuels/as-gallons E-J gas) 8.279335e-6)
        => true
        (approx= (fuels/as-L--100km E-J D-m gas) 0.01424576914902043)
        => true
        (approx= (fuels/as-mpg E-J D-m gas) 16511.188751749996)
        => true))
