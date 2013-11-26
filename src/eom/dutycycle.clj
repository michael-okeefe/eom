;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.dutycycle
  (:require [eom.constants :as constants]
            [eom.numeric :as numeric]
            [eom.resources :as rsrc]))

(def example-dutycycle
  {:name "Example"
   :times-s [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]
   :speeds-m--s [0.0 0.1 0.2 0.4 0.7 1.0 1.2 0.9 0.5 0.2 0.0]
   :elevations-m [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]})

(defn num-samples
  "[dc] -> non-negative-int
  The number of sample points in the duty cycle
  data. A sample point is an instant in time where metrics
  such as elapsed time, current speed, and current elevation
  are measured"
  [dc]
  (-> dc :times-s count))

(defn num-transitions
  "[dc] -> non-negative-int
  The number of transition steps in the duty cycle data. A
  transition step represents the change from on sample point
  to the next."
  [dc]
  (max (dec (num-samples dc)) 0))

(defn total-time-s
  "[dc] -> non-negative-int
  The total elapsed time for the duty cycle (s)"
  [dc]
  (let [times-s (:times-s dc)]
    (- (last times-s) (first times-s))))

(defn time-steps-s
  "[dc] -> seq-of-non-negative-number
  Time step durations for times of the duty cycle (s)"
  [dc]
  (numeric/diff (:times-s dc)))

(defn mid-step-times-s
  "(dc:DutyCycle) -> [ElapsedTime-s]
  Returns a vector of the elapsed times from start to the
  mid-point (or mid-step) of the time-steps. This gives the
  best representative time for a time step. Good for use of
  plotting time-step average values against."
  [dc]
  (first (reduce (fn [[ts t last-dt] dt]
                   (let [new-t (+ t (* 0.5 last-dt) (* 0.5 dt))]
                     [(conj ts new-t)
                      new-t
                      dt]))
                 [[] 0.0 0.0]
                 (time-steps-s dc))))

(defn speed-diffs-m--s
  "[dc] -> seq-of-float
  Return the differences between subsequent sampled speeds (m/s)"
  [dc]
  (numeric/diff (:speeds-m--s dc)))

(defn elevation-diffs-m
  "[dc] -> seq-of-float
  Return the difference between subsequent elevations (m)"
  [dc]
  (numeric/diff (:elevations-m dc)))

(defn accelerations-m--s2
  "[dc] -> seq-of-float
  Return the average accelerations for each time step (m/s2)"
  [dc]
  (numeric/div-ea (speed-diffs-m--s dc)
                  (time-steps-s dc)))

(defn average-speeds-m--s
  "[dc] -> seq-of-float
  Return the average speeds over each time step (m/s)"
  [dc]
  (numeric/averages (:speeds-m--s dc)))

(defn climbing-rates-m--s
  "[dc] -> seq-of-number
  The rate with which elevation is changing each time step (m/s)"
  [dc]
  (numeric/safe-div-ea 0.0
                       (elevation-diffs-m dc)
                       (time-steps-s dc)))

(defn average-speeds-cubed-m3--s3
  "[dc] -> seq-of-float
  This is the average of the cube of speed over each time step (m3/s3)"
  [dc]
  (numeric/average3s (:speeds-m--s dc)))

(defn speeds2-difference-m2--s2
  "[dc] -> seq-of-float
  The difference between the sampled speed squared of subsequent time
  steps (i.e., spds2_j+1 - spds2_j for j = 0 ... N-2 where N is the number of
  sample points) (m2/s2)"
  [dc]
  (let [spds2 (numeric/pow-ea (:speeds-m--s dc) 2.0)]
    (numeric/sub-ea (rest spds2) spds2)))

(defn distance-diffs-m
  "[dc] -> seq-of-number
  The distances traversed over each time step (m)"
  [dc]
  (numeric/mul-ea (average-speeds-m--s dc)
                  (time-steps-s dc)))

(defn distances-m
  "[dc] -> seq-of-number
  The distances at each sample time over the cycle (m)"
  [dc]
  (numeric/cumsum (cons 0.0 (distance-diffs-m dc))))

(defn distance-m
  "[dc] -> number
  The total distance traveled over the duty cycle (m)"
  [dc]
  (apply + (distance-diffs-m dc)))

(defn average-speed-m--s
  "[dc] -> number
  The average speed over the entire duty cycle (m/s)"
  [dc]
  (let [t (total-time-s dc)
        d (distance-m dc)]
    (/ d t)))

(defn moving-time-s
  "[dc] -> number
  The total elapsed time indicating movement (s)"
  [dc]
  (numeric/sum
    (map (fn [dt avg-spd] (if (> avg-spd 0.0) dt 0.0))
         (time-steps-s dc)
         (average-speeds-m--s dc))))

(defn stopped-time-s
  "[dc] -> number
  Amount of time during duty cycle that average speed is zero"
  [dc]
  (- (total-time-s dc) (moving-time-s dc)))

(defn characteristic-accelerations-m--s2
  "[dc] -> seq-of-float
  Characteristic accelerations over a duty cycle (m/s2)"
  [dc]
  (let [spd2s (numeric/pow-ea (:speeds-m--s dc) 2.0)
        dds (distance-diffs-m dc)
        des (elevation-diffs-m dc)
        g constants/gravity-m--s2]
    (map (fn [s2-1 s2-2 dd de]
           (if (== dd 0)
             0.0
             (+ (/ (* 0.5 (- s2-2 s2-1))
                   dd)
                (/ (* g de)
                   dd))))
         spd2s
         (rest spd2s)
         dds
         des)))

(defn characteristic-acceleration-m--s2
  "[dc] -> float
  Characteristic acceleration of a duty cycle (m/s2)"
  [dc]
  (let [es (:elevations-m dc)
        spd2s (numeric/pow-ea (:speeds-m--s dc) 2.0)
        D (distance-m dc)
        g constants/gravity-m--s2]
    (numeric/safe-div
      0.0
      (numeric/sum
        (map (fn [v2j-1 v2j ej-1 ej]
               (+ (numeric/positive (* 0.5 (- v2j v2j-1)))
                  (* g (- ej ej-1))))
             spd2s
             (rest spd2s)
             es
             (rest es)))
      D)))

(defn aerodynamic-speed2s-m2--s2
  "[dc] -> seq-of-float
  Aerodynamic speeds squared (m2/s2)"
  [dc]
  (let [spds (:speeds-m--s dc)
        avg-spds (numeric/averages spds)
        avg-spd3s (numeric/average3s spds)]
    (numeric/safe-div-ea 0.0 avg-spd3s avg-spds)))

(defn aerodynamic-speeds-m--s
  "[dc] -> seq-of-float
  Aerodynamic speeds (m/s)"
  [dc]
  (numeric/pow-ea (aerodynamic-speed2s-m2--s2 dc) 0.5))

(defn aerodynamic-speed2-m2--s2
  "[dc] -> float
  Aerodynamic speed squared for the entire duty cycle (m2/s2)"
  [dc]
  (let [spd3s (numeric/average3s (:speeds-m--s dc))
        dts (time-steps-s dc)
        D (distance-m dc)]
    (/ (numeric/sum (numeric/mul-ea spd3s dts))
       D)))

(defn aerodynamic-speed-m--s
  "[dc] -> float
  Aerodynamic speed for the entire duty cycle (m/s)"
  [dc]
  (java.lang.Math/pow
    (aerodynamic-speed2-m2--s2 dc) 0.5))

(defn microtrips
  "[dc] -> seq-of-dc
  Return a sequence of microtrips from the original DutyCycle"
  [dc]
  (let [n (:name dc)
        ts (:times-s dc)
        num-pts (count ts)
        ss (:speeds-m--s dc)
        es (:elevations-m dc)
        dss (speed-diffs-m--s dc)
        tol 1e-6
        is-start-stop? (fn [spd ds-before ds-after]
                         (and (< spd tol)
                              (<= ds-before tol)
                              (> ds-after tol)))
        start-stop-flags (map is-start-stop? ss (cons 0 dss) dss)
        start-stop-idxs (->>
                          (numeric/select-by-mask (range 0 num-pts)
                                                  start-stop-flags)
                          (#(conj % (dec num-pts)))
                          (cons 0)
                          distinct
                          sort)
        start-stop-idx-pairs (partition 2 1 start-stop-idxs)]
    (map (fn [[start-idx stop-idx]]
           (let [t0 (nth ts start-idx)
                 stop-idx+1 (inc stop-idx)]
             {:name n
              :times-s (map #(- % t0) (subvec ts start-idx stop-idx+1))
              :speeds-m--s (subvec ss start-idx stop-idx+1)
              :elevations-m (subvec es start-idx stop-idx+1)}))
         start-stop-idx-pairs)))

(defn splice
  "[seq-of-dc] -> dc
  Splices a seq of microtrips (duty cycles) into a single duty cycle"
  [mts]
  (letfn [(reduce-mt [dc mt]
                     (let [t-max (total-time-s dc)]
                       {:name (:name dc)
                        :times-s (vec (concat (:times-s dc)
                                              (rest (map #(+ t-max %)
                                                        (:times-s mt)))))
                        :speeds-m--s (vec (concat (:speeds-m--s dc)
                                                  (rest (:speeds-m--s mt))))
                        :elevations-m
                        (vec (concat (:elevations-m dc)
                                     (rest (:elevations-m mt))))}))]
    (reduce reduce-mt mts)))

(defn- dc-check
  "[dc] -> (union str nil)
  Check if a DutyCycle is valid. Returns nil if no errors,
  otherwise a string describing the problem."
  [dc]
  (let [ts (:times-s dc)
        ss (:speeds-m--s dc)
        es (:elevations-m dc)
        des (elevation-diffs-m dc)
        ds (distance-diffs-m dc)]
    (cond
      (= (count ts) 0) "times vector must not be empty"
      (not= (count ts) (count ss))
        "times and speeds vectors must be same length"
      (not= (count ts) (count es))
        "times and elevations vectors must be same length"
      (not (apply < ts)) "times must be increasing"
      (not (every? #(or (> % 0) (== % 0)) ts))
        "all times must be greater than or equal to zero"
      (not (every? #(or (> % 0) (== % 0)) ss))
        "all speeds must be greater than or equal to zero"
      (not (== (first ts) 0)) "time zero must be zero"
      (not (every? true? (map #(>= %1 %2) ds (map numeric/abs des))))
        (str "distance moved per time step must be greater "
             "than change in elevation")
      :otherwise nil)))

(defn to-statepoints
  "(dc:DutyCycle) -> [StatePoint]
  Turns a dutycycle into a vector of StatePoint objects."
  [dc]
  (let [ts (:times-s dc)
        ss (:speeds-m--s dc)
        es (:elevations-m dc)]
    (map (fn [t s e]
           {:time-s t
            :speed-m--s s
            :elevation-m e
            :step-wise {}
            :flags #{}})
         ts
         ss
         es)))

(defn mk-dutycycle
  "[str seq-of-number seq-of-number seq-of-number] -> dc
  Create a duty cycle from name (string), times (sequence of
  non-negative increasing numbers in seconds since start),
  speeds (sequence of non-negative numbers in m/s), and
  elevations (sequence of numbers as height above some reference
  point -- usually sea level -- in meters)."
  [name times-s speeds-m--s elevations-m]
  {:pre [(string? name) (every? number? times-s)
         (every? number? speeds-m--s)
         (or (every? number? elevations-m)
             (nil? elevations-m))]}
  (let [dc {:name name
            :times-s (mapv #(double %) times-s)
            :speeds-m--s (mapv #(double %) speeds-m--s)
            :elevations-m (if (nil? elevations-m)
                            (repeat (count times-s) 0.0)
                            (mapv #(double %) elevations-m))}
        checks (dc-check dc)]
    (if (nil? checks)
      dc
      (throw (Exception. checks)))))

(defn load-cycle
  "path -> dc
  Load a DutyCycle from the given path and return it."
  [path-to-cycle]
  (rsrc/load-edn path-to-cycle
                 {'eom/dutycycle
                  (fn [{:keys [name times-s speeds-m--s elevations-m]}]
                    (mk-dutycycle name times-s speeds-m--s elevations-m))}))
