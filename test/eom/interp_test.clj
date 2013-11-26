;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.interp-test
  (:use [clojure.test :only [deftest is are testing]])
  (:require [eom.interp :as T]))

(deftest interp
  (let [m (T/mk-model [1 2 3 4] [10 20 30 40])]
    (testing "linear-1d"
      (is (== 30.0 (T/linear-1d m 3)))
      (is (== 40.0 (T/linear-1d m 4)))
      (is (== 24.0 (T/linear-1d m 2.4)))
      (is (nil? (T/linear-1d m 0.5)))
      (is (nil? (T/linear-1d m 4.5))))
    (testing "linead-1d with edge extension"
      (is (== 30.0 (T/with-edge-extension T/linear-1d m 3)))
      (is (== 40.0 (T/with-edge-extension T/linear-1d m 4)))
      (is (== 24.0 (T/with-edge-extension T/linear-1d m 2.4)))
      (is (== 10.0 (T/with-edge-extension T/linear-1d m 0.5)))
      (is (== 40.0 (T/with-edge-extension T/linear-1d m 4.5))))))
