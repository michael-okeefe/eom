;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.constants)

(def
  ^{:ref-url "http://en.wikipedia.org/wiki/Density_of_air"}
  gravity-m--s2 9.80665)

(def
  ^{:ref-url "http://en.wikipedia.org/wiki/Density_of_air"}
  sea-level-standard-atmospheric-pressure-kPa 101.325)

(def
  ^{:ref-url "http://en.wikipedia.org/wiki/Density_of_air"}
  sea-level-standard-temperature-K 288.15)

(def
  ^{:ref-url "http://en.wikipedia.org/wiki/Density_of_air"}
   temperature-lapse-rate-K--m 0.0065)

(def
  ^{:ref-url "http://en.wikipedia.org/wiki/Gas_constant"}
  ideal-gas-constant-J--mol-K 8.3144621)
