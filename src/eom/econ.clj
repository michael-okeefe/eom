;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.econ)

(defn F<-P
  "(P:Real i:Real n:Real) -> Real
  Find the future value given present
  - P :: present amount
  - i :: interest rate (should be 0 <= i <= 1)
  - n :: number of periods
  Return :: future value of present amount"
  [P i n]
  (let [c (Math/pow (+ 1.0 i) n)]
    (* P c)))

(defn P<-F
  "(F:Real i:Frac n:Real) -> Real
  Find the present value given a future amount
  - F :: future amount
  - i :: interest rate (normally 0 <= i <= 1)
  - n :: number of periods
  Return :: present value of future amount"
  [F i n]
  (let [d (Math/pow (+ 1.0 i) (- n))]
    (* F d)))

(defn F<-A
  "(A:Real i:Frac n:Real) -> Real
  Find the future value of a recurring amount per period
  - A :: recurring amount per period
  - i :: the interest rate (normally 0 <= i <= 1)
  - n :: the number of periods
  Return :: the future value of a recurring amount
  per period, A"
  [A i n]
  (let [c (Math/pow (+ 1.0 i) n)]
    (if (== i 0)
      (* A n)
      (* A (/ (- c 1.0)
              i)))))

(defn A<-F
  "(F:Real i:Frac n:Real) -> Real
  Find the recurring amount equal to a future value
  - F :: the future amount
  - i :: the interest rate (normally 0 <= i <= 1)
  - n :: the number of periods
  Return :: the value of recurring payments per period
  equal to the future value"
  [F i n]
  (let [c (Math/pow (+ 1.0 i) n)]
    (if (== i 0)
      (/ F n)
      (* F (/ i
              (- c 1.0))))))

(defn A<-P
  "(P:Real i:Frac n:Real) -> Real
  Find the recurring amount per period equal in value to
  a present amount
  - P :: the present amount
  - i :: the interest rate (usually 0 <= i <= 1)
  - n :: the number of periods
  Return :: the value of recurring payments per
  period equal in value to the present amount"
  [P i n]
  (let [c (Math/pow (+ 1.0 i) n)]
    (if (== i 0)
      (/ P n)
      (* P (/ (* i c)
              (- c 1.0))))))

(defn P<-A
  "(A:Real i:Frac n:Real) -> Real
  The present value of a series of amounts per period
  - A :: the amount per period
  - i :: the interest rate per period (usually 0 <= i <= 1)
  - n :: the number of periods
  Returns :: the present value of a series of n amounts per
  period at an interest rate of i per period"
  [A i n]
  (let [c (Math/pow (+ 1.0 i) n)]
    (if (== i 0)
      (* A n)
      (* A (/ (- c 1.0)
              (* i c))))))
