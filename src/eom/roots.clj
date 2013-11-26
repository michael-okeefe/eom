;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; This module is for generic root finding algorithms which find the value
;; of x (a scalar) that solves:
;;
;; (f x) => 0.0
;;
;; References:
;; [1] Brent, R. (1973). "Algorithms for minimization without Derivatives".
;;     Prentice Hall. Englewood Cliffs, NJ. Chapters 3,4.
;; [2] Wikipedia http://en.wikipedia.org/wiki/Brent's_method
;; [3] Stage, S. (2013). "Comments on An Improvement to Brent's Method".
;;     International Journal of Experimental Algorithms (IJEA). Vol. 4 No 1.
;; [4] Regula Falsi overview
;; http://www.physics.arizona.edu/~restrepo/475A/Notes/sourcea-/node17.html
;; [5] Kiusalaas, J. (2005). "Numerical Methods in Engineering with Python."
;;     First Edition. Cambridge University Press. New York, NY. pp. 148-151.
;; [6] "Van Wijngaarden-Dekker-Brent Method". Numerical Recipies. pp 454-456.
(ns eom.roots)

(defn throw-if-not-bracketed
  "(fa:Real * fb:Real) -> nil
  fa and fb are (f a) and (f b) respectively.
  Throw an error if fa and fb have the same sign
  (indicating that there is no root between fa and fb)"
  [fa fb]
  (if (> (* fa fb) 0.0)
    (throw (Exception. "Root not bracketed"))))

(defn same-signs?
  "(a:Number * b:Number) -> Boolean
  Returns true if a and b have the same signs"
  [a b]
  (or (and (> a 0.0) (> b 0.0))
      (and (< a 0.0) (< b 0.0))))

(def opposite-signs? (complement same-signs?))

(defn ensure-ordering
  "(a:Number * b:Number) -> [Number Number]
  Orders a and b such that a <= b"
  [a b]
  (if (> a b) [b a] [a b]))

(defn converged?
  "(a:Real * b:Real * fa:Real * fb:Real * tol:Real) -> Bool"
  [a b fa fb tol]
  (let [abs-b-a (Math/abs (- b a))]
    (or (<= abs-b-a tol)
        (<= (Math/abs fa) tol)
        (<= (Math/abs fb) tol)
        (<= abs-b-a
            (* 0.5 (Math/abs (+ a b)) tol)))))

;; Reference [6] pps. 454-455
(defn guess-root-by-inverse-quadratic-interpolation
  "(a:Real * fa:Real * b:Real * fb:Real * c:Real * fc:Real) -> Real
  Given three points in [x, y], specifically [a,fa], [b,fb], and
  [c,fc], fit a quadratic to x(f) and calculate x(f=0)"
  [a fa b fb c fc]
  (let [R (/ fb fc)
        S (/ fb fa)
        T (/ fa fc)
        P (* S (- (* T (- R T) (- c b))
                  (* (- 1.0 R) (- b a))))
        Q (* (- T 1.0) (- R 1.0) (- S 1.0))]
    (+ b (/ P Q))))

(defn guess-root-by-secant-method
  "(a:Real * fa:Real * b:Real * fb:Real) -> Real
  Given two points, guess the next root by using the
  secant method (using a line between the two points,
  interpolate or extrapolate for the zero crossing)"
  [a fa b fb]
  (+ a (/ (* (- a b) fa) (- fb fa))))

(defn guess-root-by-bisection
  "(a:Real * b:Real) -> Real
  Given two bounds, guess the location of the root by
  bisecting the bounding range"
  [a b]
  (/ (+ a b) 2.0))

(defn bisection
  "(f:(Fn Number -> Number) * a:Number * b:Number * tol:Number) -> Number
  Find the root of function f between bounds a and b to the given tolerance,
  tol."
  [f a b tol]
  (let [[a b] (ensure-ordering a b)
        fa (f a)
        fb (f b)
        max-iter 500]
    (throw-if-not-bracketed fa fb)
    (loop [a a b b fa fa fb fb iter 0]
      (let [c (guess-root-by-bisection a b)
            fc (f c)]
        (cond
         (or (== fc 0.0) (converged? a b fa fb tol))
         {:root c :value fc :iter iter :calls (+ 3 iter)}
         (> iter max-iter)
         (throw (Exception. "bisection :: maximum iterations reached"))
         :else
         (if (opposite-signs? fa fc)
           (recur a c fa fc (inc iter))
           (recur c b fc fb (inc iter))))))))

;; Reference [4] with improvements from [3]
(defn regula-falsi
  "(f:(Fn Real -> Real) * a:Real * b:Real * tol:Real) -> Real
  Determine the root of the function f in bounds a to b
  by the Regula Falsi (false position) method."
  [f a b tol]
  (let [[a b] (ensure-ordering a b)
        fa (f a)
        fb (f b)
        eps 1e-16
        max-iter 500]
    (throw-if-not-bracketed fa fb)
    (loop [a a b b fa fa fb fb iend -1 iter 0]
      (let [c (guess-root-by-secant-method a fa b fb)
            fc (f c)]
        (cond
         (or (== fc 0.0) (converged? a b fa fb tol))
         {:root c :value fc :iter iter :calls (+ 3 iter)}
         (> iter max-iter)
         (throw (Exception. "regula-falsi :: max iterations reached"))
         :otherwise
         (if (<= (* fa fc) 0.0)
           (recur a c (if (= iend 0) (* fa 0.5) fa) fc 0 (inc iter))
           (recur c b fc (if (= iend 1) (* fb 0.5) fb) 1 (inc iter))))))))

;; reference [5]
(defn brent-simplified
  "(f:(Fn Number -> Number) * a:Number * b:Number * tol:Number) -> Number
  Find the root of function f between bounds a and b to the given tolerance,
  tol."
  [f a b tol]
  (let [[a b] (ensure-ordering a b)
        fa (f a)
        max-iter 100]
    (if (== fa 0.0)
      {:root a :value fa :iter 0 :calls 1}
      (let [fb (f b)]
        (if (== fb 0.0)
          {:root b :value fb :iter 0 :calls 2}
          (do
            (throw-if-not-bracketed fa fb)
            (loop [a a b b c (* 0.5 (+ a b)) fa fa fb fb iter 0]
              (let [fc (f c)]
                (cond
                 (or (== fc 0.0) (converged? a b fa fb tol))
                 {:root c :value fc :iter iter :calls (+ 3 iter)}
                 (> iter max-iter)
                 (throw
                  (Exception. "brent-simplified :: max iterations exceeded"))
                 :else-try-quadratic-interpolation
                 (let [denom (* (- fb fa) (- fc fa) (- fb fc))
                       numer (+ (* c (- fa fb) (+ fb (- fc) fa))
                                (* fb a (- fb fc))
                                (* fa b (- fc fa)))
                       dx (if (== denom 0.0)
                            (- b a)
                            (/ (* fc numer) denom))
                       new-c (+ c dx)]
                   (if (< (* (- b new-c) (- new-c a)) 0.0)
                     (let [dx (* 0.5 (- b a))
                           new-c (+ a dx)]
                       (if (< new-c c)
                         (recur a c new-c fa fc (inc iter))
                         (recur c b new-c fc fb (inc iter))))
                     (if (< new-c c)
                       (recur a c new-c fa fc (inc iter))
                       (recur c b new-c fc fb (inc iter))))))))))))))

(defn magnitude-with-sign
  "(x:Number * y:Number) -> Number
  Returns the magnitude of x with the sign of y"
  [x y]
  (if (or (and (> x 0.0) (> y 0.0))
          (and (< x 0.0) (< y 0.0))
          (and (== x 0.0) (== y 0.0)))
    x
    (- x)))

;; reference [6]
(defn brent
  "(f:(Fn Number -> Number) * a:Number * b:Number * tol:Number) -> Number
  Find the root of function f between bounds a and b to the given tolerance,
  tol."
  [f a b tol]
  (let [max-iter 100
        eps 1e-16
        c b
        fa (f a)
        fb (f b)
        d (- b a)
        e d]
    (throw-if-not-bracketed fa fb)
    (loop [a a b b c c d d e e fa fa fb fb fc fb iter 0]
      (cond
       (> iter max-iter)
       (throw (Exception. "Max iterations reached on brent"))
       (or (and (> fb 0.0) (> fc 0.0))
           (and (< fb 0.0) (< fc 0.0)))
       (recur a b a (- b a) (- b a) fa fb fa iter)
       (< (Math/abs fc) (Math/abs fb))
       (recur b c a d e fb fc fa iter)
       :otherwise
       (let [tol1 (+ (* 2.0 eps (Math/abs b)) (* 0.5 tol))
             xm (* 0.5 (- c b))]
         (if (or (<= (Math/abs xm) tol1)
                 (== fb 0.0))
           {:root b :value fb :iter iter :calls (+ 2 iter)}
           (if (and (>= (Math/abs e) tol1)
                    (> (Math/abs fa) (Math/abs fb)))
             (let [s (/ fb fa)
                   [p_ q_] (if (== a c)
                             [(* 2.0 xm s) (- 1.0 s)]
                             (let [q0 (/ fa fc)
                                   r (/ fb fc)]
                               [(* s (- (* 2.0 xm q0 (- q0 r))
                                        (* (- b a) (- r 1.0))))
                                (* (- q0 1.0) (- r 1.0) (- s 1.0))]))
                   q (if (> p_ 0.0) (- q_) q_)
                   p (Math/abs p_)
                   min1 (- (* 3.0 xm q) (Math/abs (* tol1 q)))
                   min2 (Math/abs (* e q))
                   new-d (if (< (* 2.0 p) (min min1 min2))
                           (/ p q)
                           xm)
                   new-e d
                   new-b (if (> (Math/abs new-d) tol)
                           (+ b new-d)
                           (+ b (magnitude-with-sign tol1 xm)))]
               (recur b
                      new-b
                      c
                      new-d
                      new-e
                      fb
                      (f new-b)
                      fc
                      (inc iter)))
             (let [new-d xm
                   new-e d
                   new-b (if (> (Math/abs new-d) tol)
                           (+ b new-d)
                           (+ b (magnitude-with-sign tol1 xm)))]
               (recur b
                      new-b
                      c
                      new-d
                      new-e
                      fb
                      (f new-b)
                      fc
                      (inc iter))))))))))

(defn guess-root-by-zhang-method
  "(a:Real * fa:Real * b:Real * fb:Real * c:Real * fc:Real) -> Real
  Return an estimate for the next root by an advanced method"
  [a fa b fb c fc]
  {:pre [(<= (* fa fb) 0.0)]}
  (if (and (not (== fa fc)) (not (== fb fc)) (not (== fa fb)))
    (let [s (guess-root-by-inverse-quadratic-interpolation a fa b fb c fc)]
      (if (and (< a s) (< s b))
        s
        c))
    (if (< (* fa fc) 0.0)
      (guess-root-by-secant-method a fa c fc)
      (guess-root-by-secant-method c fc b fb))))

(guess-root-by-secant-method 0.0 -2.0 2.0 2.0)

;; reference [3]
(defn zhang
  "(f:(Fn Number -> Number) * a:Number * b:Number * tol:Number) -> Number
  Find the root of function f between bounds a and b to the given tolerance,
  tol."
  [f a b tol]
  (let [[a b] (ensure-ordering a b)
        fa (f a)
        fb (f b)
        max-iter 100]
    (throw-if-not-bracketed fa fb)
    (loop [a a b b fa fa fb fb iter 0]
      (let [c (guess-root-by-bisection a b)
            fc (f c)]
        (cond
         (converged? a b fa fb tol)
         (let [[root froot] (let [fmin (min (Math/abs fa) (Math/abs fb) (Math/abs fc))]
                              (cond (== fmin (Math/abs fa)) [a fa]
                                    (== fmin (Math/abs fb)) [b fb]
                                    :else [c fc]))]
           {:root root :value froot :iter iter :calls (+ 4 (* 2 iter))})
         (> iter max-iter)
         (throw (Exception. "zhang :: maximum iterations exceeded"))
         :else
         (let [s (guess-root-by-zhang-method a fa b fb c fc)
               fs (if (== s c) fc (f s))
               [s fs c fc] (if (> c s) [c fc s fs] [s fs c fc])
               next-iter (inc iter)]
           (cond
            (<= (* fc fs) 0.0)
            (recur c s fc fs next-iter)
            (< (* fa fc) 0.0)
            (recur a c fa fc next-iter)
            :else-between-s-and-b
            (recur s b fs fb next-iter))))))))

(defn find-root
  "(f:(Fn Real -> Real) * a:Real * b:Real) -> Real
  |(f:(Fn Real -> Real) * a:Real * b:Real * tol:Real) -> Real
  |(f:(Fn Real -> Real) * a:Real * b:Real * tol:Real * method:Keyword) -> Real
  Find the root of function f between bounds a and b to the given tolerance
  (defaults to 1e-15) using the indicated method (defaults to :brent)."
  ([f a b]
   (find-root f a b 1e-15 :brent))
  ([f a b tol]
   (find-root f a b tol :brent))
  ([f a b tol method]
   (let [methods {:brent brent
                  :brent-simplified brent-simplified
                  :bisection bisection
                  :regula-falsi regula-falsi
                  :zhang zhang}]
     (if (contains? methods method)
       (:root ((get methods method) f a b tol))
       (throw (Exception. (str "Unknown method " method)))))))

(comment
  (def f0 (fn [x] (- (* 2.0 x x) 8.0)))
  (def f1 (fn [x] (- (Math/cos x) (* x x x))))
  (def f2 (fn [x] (- (Math/cos x) x)))
  (def f3 (fn [x] (- 1.0 (* 0.75 x))))
  (def f4 (fn [x] (if (<= x 2/3)
                    (- (Math/sqrt (Math/abs (- x (/ 2.0 3.0)))) 0.1)
                    (- (- (Math/sqrt (Math/abs (- x (/ 2.0 3.0))))) 0.1))))
  (def f5 (fn [x] (if (<= x 2/3)
                    (Math/pow (Math/abs (- x (/ 2.0 3.0))) 0.2)
                    (- (Math/pow (Math/abs (- x (/ 2.0 3.0))) 0.2)))))

  (bisection f0 0.0 10.0 1e-15)
  (bisection f1 -10.0 10.0 1e-15)
  (bisection f2 -10.0 10.0 1e-15)
  (bisection f3 -10.0 10.0 1e-15)
  (bisection f4 -10.0 10.0 1e-15)
  (bisection f5 -10.0 10.0 1e-15)

  (regula-falsi f0 0.0 10.0 1e-15)
  (regula-falsi f1 -10.0 10.0 1e-15)
  (regula-falsi f2 -10.0 10.0 1e-15)
  (regula-falsi f3 -10.0 10.0 1e-15)
  (regula-falsi f4 -10.0 10.0 1e-15)
  (regula-falsi f5 -10.0 10.0 1e-15)

  (brent-simplified f0 0.0 10.0 1e-15)
  (brent-simplified f1 -10.0 10.0 1e-15)
  (brent-simplified f2 -10.0 10.0 1e-15)
  (brent-simplified f3 -10.0 10.0 1e-15)
  (brent-simplified f4 -10.0 10.0 1e-15)
  (brent-simplified f5 -10.0 10.0 1e-15)

  (brent f0 0.0 10.0 1e-15)
  (brent f1 -10.0 10.0 1e-15)
  (brent f2 -10.0 10.0 1e-15)
  (brent f3 -10.0 10.0 1e-15)
  (brent f4 -10.0 10.0 1e-15)
  (brent f5 -10.0 10.0 1e-15)

  (zhang f0 0.0 10.0 1e-15)
  (zhang f1 -10.0 10.0 1e-15)
  (zhang f2 -10.0 10.0 1e-15)
  (zhang f3 -10.0 10.0 1e-15)
  (zhang f4 -10.0 10.0 1e-15)
  (zhang f5 -10.0 10.0 1e-15)

  (find-root f5 -10.0 10.0 1e-15 :brent)
)

