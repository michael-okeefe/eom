;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; This module specifies components that can be used for a powerflow powertrain
;; model.
(ns eom.powerflow
  (:require [clojure.set :as cset]
            [eom.utils :refer [safe-get]]
            [eom.interp :as interp]
            [eom.dutycycle :as dc]
            [eom.load :as ld]))

(defprotocol Comp
  (step [c power-out-request-W dtime-s]
   "(c:Comp * power-out-request-W:Real * dtime-s:Real)
   -> {:comp Comp :pwr-out-W Real}
   Return the state of the component and power delivered
   after one time step for dtime-s seconds. Use component,
   c, to provide the power out requested in W
   (power-out-request-W) for one time step of duration
   dtime-s.
   - c :: a Comp
   - power-out-request-W :: average power out requested;
   a positive or negative Real number. Positive indicates
   power output (motoring) while negative indicates power
   absorbed (braking)
   - dtime-s :: the duration of the time step
   RETURNS :: a map with the following keys:
   {:comp <the possibly updated version of c>
    :pwr-out-W <the power out achieved in W>}")
  (max-power-out-W [c dtime-s]
   "(c:Comp dtime-s:Real) -> Real
   Provide the maximum power output capability of the current component
   over the given time duration of dtime-s.
   - c :: the upstream component in question
   - dtime-s :: the elapsed time of the next
   time-step
   RETURNS the max-power-out (W) calculated as a number.")
  (min-power-out-W [c dtime-s]
   "(Comp, Real) -> Real
   Determine the maximum power output
   - c :: component in question
   - dtime-s :: the time step duration (s)
   Return :: maximum power output (W)"))

(defn bound
  "(n:Real, max:Real, min:Real) -> Real
  - n :: number to test
  - max :: the upper limit
  - min :: the lower limit
  Returns :: if (min <= n <= max) -> n
             n > max -> max
             n < min -> min"
  [n max min]
  {:pre [(number? n) (number? max) (number? min)
         (>= max min)]
   :post [#(number? %)]}
  (if (> n max)
    max
    (if (< n min)
      min
      n)))

(defn make-split-power-thermostat-controller
  "(Fn (Fn Comp -> Real) Real Real ->
       (Fn Comp Real [Real] [Real] Real -> [Real]))
  Return a function for doing power split control that
  adds a charging load on the first component to charge
  the second."
  [soe-getter soe-target weight]
  (fn [c pwr-in-req-W maxs-W mins-W dt-s]
    (let [soe (soe-getter c)
          [primary-max-W secondary-max-W] maxs-W
          [primary-min-W secondary-min-W] mins-W
          secondary-out-W (bound (* weight (- soe soe-target))
                             secondary-max-W
                             secondary-min-W)
          P-primary-W (bound (- pwr-in-req-W secondary-out-W)
                             primary-max-W
                             primary-min-W)
          P-secondary-W (bound (- pwr-in-req-W P-primary-W)
                               secondary-max-W
                               secondary-min-W)]
      [P-primary-W P-secondary-W])))

(defn split-power-first-favored
  "(c:Comp * pwr-in-req-W:Real * maxs-W:[Real] * mins-W:[Real] * dt-s:Real) -> [Real]
  Split input power using all the power capabilities of the first components in the
  vector prior to other components."
  [c pwr-in-req-W maxs-W mins-W dt-s]
  (safe-get (reduce (fn [state [max-W min-W]]
                      (let [p (bound (- pwr-in-req-W
                                        (safe-get state :total-W))
                                     max-W
                                     min-W)]
                        (-> state
                            (update-in [:total-W] + p)
                            (update-in [:powers-W] conj p))))
                    {:total-W 0.0 :powers-W []}
                    (map vector maxs-W mins-W))
            :powers-W))

(defn split-power-evenly
  "(c:Comp * pwr-in-req-W:Real * maxs-W:[Real] * mins-W:[Real] * dt-s:Real) -> [Real]
  Split input power evenly while taking into account
  the given component's power limits. Practically, this tries to split the load
  evenly and attenuates the loads based on power limits."
  [c pwr-in-req-W maxs-W mins-W dt-s]
  (let [num-ins (count (safe-get c :inputs))
        ps (if (= num-ins 0)
             []
             (repeat num-ins (/ pwr-in-req-W (double num-ins))))
        limited-ps (map #(bound %1 %2 %3) ps maxs-W mins-W)
        current-W (reduce + limited-ps)]
    (if (== current-W pwr-in-req-W)
      limited-ps
      (let [total-possible-W (if (>= pwr-in-req-W 0.0)
                               (reduce + maxs-W)
                               (reduce + mins-W))
            availables-W (if (>= pwr-in-req-W 0.0)
                           (map - maxs-W limited-ps)
                           (map - mins-W limited-ps))
            total-available-W (reduce + availables-W)
            add-fraction (if (== total-available-W 0.0)
                           0.0
                           (min (/ (- pwr-in-req-W current-W)
                                   total-available-W)
                                1.0))]
        (map (fn [p avail] (+ p (* add-fraction avail)))
             limited-ps
             availables-W)))))

(defn make-log
  "() -> Log
  Create a new power/energy logger"
  []
  {:elapsed-time-s []
   :power-out-W []
   :power-in-W []
   :power-loss-W []
   :energy-stored-J []})

(defn components
  "(Fn Comp -> [Comp])
  Return a sequence of all the components in a powertrain"
  [c]
  (loop [this-c c cs []]
    (if (contains? this-c :input)
      (recur (safe-get this-c :input) (conj cs this-c))
      (if (contains? this-c :inputs)
        (let [new-cs (mapcat components (safe-get this-c :inputs))]
          (concat cs [this-c] new-cs))
        (conj cs this-c)))))

(defn energy-balance
  "(Fn Comp -> [{:unbalance-J Real :loss-J Real :stored-J Real :out-J Real}])
  For each time step, calculate the total loss, change in storage, and output
  energy. Track any unbalance in the terms."
  [c]
  (let [out (reduce (fn [states cmpt]
                      (let [log (safe-get cmpt :log)]
                        (map (fn [state loss-J stored-J]
                               (merge-with +
                                           state
                                           {:loss-J loss-J
                                            :stored-J stored-J}))
                             states
                             (map *
                                  (safe-get log :power-loss-W)
                                  (safe-get log :elapsed-time-s))
                             (safe-get log :energy-stored-J)
                             )))
                    (map (fn [Pout-W dt] {:out-J (* Pout-W dt)})
                         (get-in c [:log :power-out-W])
                         (get-in c [:log :elapsed-time-s]))
                    (components c))]
    (mapv (fn [{:keys [loss-J stored-J out-J] :as m}]
            (assoc m :unbalance-J (+ loss-J out-J stored-J)))
          out)))

(defn unbalanced?
  "(Fn Comp -> [Int])
  Returns indicies where the energy balance for a component
  doesn't work out correctly based on the log."
  ([c] (unbalanced? c 1e-6))
  ([c tol]
   (let [log (safe-get c :log)
         dts (safe-get log :elapsed-time-s)
         Eouts-J (map * (safe-get log :power-out-W) dts)
         Eins-J (map * (safe-get log :power-in-W) dts)
         Elosses-J (map * (safe-get log :power-loss-W) dts)
         Estoreds-J (safe-get log :energy-stored-J)
         unbalanced-J (map + Eins-J
                           (map - Eouts-J)
                           (map - Elosses-J)
                           (map - Estoreds-J))]
     (:idxs (reduce (fn [{:keys [idxs idx] :as m} u]
                      (if (> (Math/abs u) tol)
                        (assoc m
                          :idxs (conj idxs (inc idx))
                          :idx (inc idx))
                        (assoc m :idx (inc idx))))
                    {:idx -1 :idxs []}
                    unbalanced-J)))))

(defn update-log
  "Update the power/energy logger"
  [log dt-s pwr-out-W pwr-in-W pwr-loss-W e-stored-J]
  (-> log
      (update-in [:elapsed-time-s] conj dt-s)
      (update-in [:power-out-W] conj pwr-out-W)
      (update-in [:power-in-W] conj pwr-in-W)
      (update-in [:power-loss-W] conj pwr-loss-W)
      (update-in [:energy-stored-J] conj e-stored-J)))

(defn generic-step
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real *
    pwr-in-req-fun:(Fn c:Comp * pwr-out-req-W:Real * dt-s:Real
                    -> pwr-in-req-W:Real) *
    pwr-out-ach-fun:(Fn c:Comp * pwr-out-req-W:Real * dt-s:Real *
                    pwr-in-ach-W:Real -> pwr-out-ach-W:Real))
    -> {:comp Comp :pwr-out-W Real}"
  [c pwr-out-req-W dt-s pwr-in-req-fun pwr-out-ach-fun]
  (let [pwr-in-req-W (pwr-in-req-fun c pwr-out-req-W dt-s)
        {new-input :comp pwr-in-ach-W :pwr-out-W}
        (step (safe-get c :input) pwr-in-req-W dt-s)
        pwr-out-ach-W (pwr-out-ach-fun c pwr-out-req-W dt-s pwr-in-ach-W)
        pwr-loss-ach-W (Math/abs (- pwr-in-ach-W pwr-out-ach-W))
        new-c (-> (safe-get c :log)
                  (update-log
                   dt-s
                   pwr-out-ach-W
                   pwr-in-ach-W
                   pwr-loss-ach-W
                   0.0)
                  (->> (assoc c :log))
                  (assoc :input new-input))]
    {:comp new-c
     :pwr-out-W pwr-out-ach-W}))

(defn const-eff-power-in-request-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real) -> pwr-in-req-W:Real
  Calculates the power in requested (in Watts) based on a constant
  efficiency."
  [c pwr-out-req-W dt-s]
  (let [eff (safe-get c :eff)]
    (if (>= pwr-out-req-W 0.0)
      (/ pwr-out-req-W eff)
      (* pwr-out-req-W eff))))

(defn const-eff-power-out-achieved-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real * pwr-in-ach-W:Real)
  -> pwr-out-ach-W:Real
  Calculate the power out achieved given power in achieved."
  [c pwr-out-req-W dt-s pwr-in-ach-W]
  (let [eff (safe-get c :eff)]
    (if (>= pwr-in-ach-W 0.0)
      (* pwr-in-ach-W eff)
      (/ pwr-in-ach-W eff))))

;; Constant Efficiency Component
(defrecord ConstEffComp [id eff input log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (generic-step c
                      pwr-out-req-W
                      dt-s
                      const-eff-power-in-request-W
                      const-eff-power-out-achieved-W))
  (max-power-out-W
    [c dt-s]
    (let [eff (safe-get c :eff)
          max-pwr-in-W (max-power-out-W (safe-get c :input) dt-s)]
      (if (>= max-pwr-in-W 0.0)
        (* eff max-pwr-in-W)
        (/ max-pwr-in-W eff))))
  (min-power-out-W
    [c dt-s]
    (let [eff (:eff c)
          min-pwr-in-W (min-power-out-W (safe-get c :input) dt-s)]
      (if (>= min-pwr-in-W 0.0)
        (* eff min-pwr-in-W)
        (/ min-pwr-in-W eff)))))

(defn make-constant-efficiency-component
  "(id:Keyword * eff:RealFraction * input:Comp * log:Map Keyword Vector)
  -> ConstEffComp
  | (id:Keyword * eff:RealFraction * input:Comp) -> ConstEffComp
  Create a constant efficiency component"
  ([id eff input]
   (make-constant-efficiency-component id eff input (make-log)))
  ([id eff input log]
   {:pre [(keyword? id)
          (number? eff)
          (and (>= eff 0.0) (<= eff 1.0))]}
   (ConstEffComp. id eff input log)))

;; Infinite Source Component
(defrecord InfSrcComp
  [id log]
  Comp
  (step
    [c pwr-out-req-W dt-s]
    (let [pwr-out-ach-W (if (>= pwr-out-req-W 0.0)
                           pwr-out-req-W
                           0.0)
          new-c (-> (safe-get c :log)
                    (update-log
                     dt-s
                     pwr-out-ach-W
                     0.0
                     0.0
                     (* -1.0 dt-s pwr-out-ach-W))
                    (->> (assoc c :log)))]
      {:comp new-c
       :pwr-out-W pwr-out-ach-W}))
  (max-power-out-W [c dt-s] Double/POSITIVE_INFINITY)
  (min-power-out-W [c dt-s] 0.0))

(defn make-infinite-source-component
  "(id:Keyword * log:Logger) -> InfSrcComp
  | (id:Keyword) -> InfSrcComp
  Create an infinite source component and return it"
  ([id]
   (make-infinite-source-component id (make-log)))
  ([id log]
   (InfSrcComp. id log)))

(defn limited-power-in-request-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real) -> pwr-in-req-W:Real
  Calculates the power in requested (in Watts) limited by this
  component's power level values."
  [c pwr-out-req-W dt-s]
  (bound pwr-out-req-W
         (safe-get c :max-pwr-out-W)
         (safe-get c :min-pwr-out-W)))

(defn limited-power-out-achieved-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real * pwr-in-ach-W:Real)
  -> pwr-out-ach-W:Real
  Calculate the power out achieved given power in achieved."
  [c pwr-out-req-W dt-s pwr-in-ach-W]
  pwr-in-ach-W)

(defrecord LimitedOutputComp
  [id max-pwr-out-W min-pwr-out-W input log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (generic-step c
                      pwr-out-req-W
                      dt-s
                      limited-power-in-request-W
                      limited-power-out-achieved-W))
  (max-power-out-W
    [c dt-s]
    (let [max-pwr-in-upstream (max-power-out-W (safe-get c :input) dt-s)]
      (min (safe-get c :max-pwr-out-W) max-pwr-in-upstream)))
  (min-power-out-W
    [c dt-s]
    (let [min-pwr-in-upstream (min-power-out-W (safe-get c :input) dt-s)]
      (max (safe-get c :min-pwr-out-W) min-pwr-in-upstream))))

(defn make-limited-output-component
  "(id:Keyword * max-pwr-out-W:Real * min-pwr-out-W:Real
    * input:Comp * log:Log) -> LimitedOutputComp
  | (id:Keyword * max-pwr-out-W:Real * min-pwr-out-W:Real
    * input:Comp) -> LimitedOutputComp
  Create a limited power output component."
  ([id max-pwr-out-W min-pwr-out-W input]
   (make-limited-output-component id max-pwr-out-W min-pwr-out-W input (make-log)))
  ([id max-pwr-out-W min-pwr-out-W input log]
   {:pre [(keyword? id)
          (number? max-pwr-out-W)
          (number? min-pwr-out-W)]}
   (LimitedOutputComp. id max-pwr-out-W min-pwr-out-W input log)))

(defn brakes-power-in-request-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real) -> pwr-in-req-W:Real
  Determine the power request INTO a braking component
  based on the power out requested. The basic strategy
  is a LAZY braking strategy. All regen energy is sent
  up the powertrain and whatever is not captured is
  captured by the brakes during calculation of achieved
  braking."
  [c pwr-out-req-W dt-s]
  pwr-out-req-W)

(defn brakes-power-out-achieved-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real * pwr-in-ach-W:Real)
  -> pwr-out-ach-W:Real
  Attempt to satisfy the power out achieved. The brake component
  will absorb negative power NOT already absorbed by the powertrain
  (i.e., negative value of pwr-in-ach-W) to the extent of min-pwr-out-W.
  Thus the total negative power absorption (i.e., braking) of the powertrain
  will be (+ min-pwr-out-W pwr-in-ach-W) when both values are negative."
  [c pwr-out-req-W dt-s pwr-in-ach-W]
  (if (>= pwr-out-req-W 0.0)
    (if (< pwr-in-ach-W 0.0)
      (if (< pwr-in-ach-W -1e-6)
        (throw (Exception. (str "Brake component :: negative power in achieved "
                                "for positive power out request..."
                                pwr-out-req-W ", " pwr-in-ach-W)))
        0.0)
      pwr-in-ach-W)
    (+ pwr-in-ach-W
       (max (- pwr-out-req-W pwr-in-ach-W)
            (safe-get c :min-pwr-out-W)))))

(defrecord BrakeComp
  [id min-pwr-out-W input log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (generic-step c
                      pwr-out-req-W
                      dt-s
                      brakes-power-in-request-W
                      brakes-power-out-achieved-W))
  (max-power-out-W
    [c dt-s]
    (let [max-pwr-in-upstream (max-power-out-W (safe-get c :input) dt-s)]
      (min (safe-get c :max-pwr-out-W) max-pwr-in-upstream)))
  (min-power-out-W
    [c dt-s]
    (let [min-pwr-in-upstream (min-power-out-W (safe-get c :input) dt-s)]
      (max (safe-get c :min-pwr-out-W) min-pwr-in-upstream))))

(defn make-brake-component
  "(id:Keyword * min-pwr-out-W:Real * input:Comp * log:Log) -> BrakeComp
  |(id:Keyword * min-pwr-out-W:Real * input:Comp) -> BrakeComp
  Create a brake component and return it."
  ([id min-pwr-out-W input]
   (make-brake-component id min-pwr-out-W input (make-log)))
  ([id min-pwr-out-W input log]
   {:pre [(number? min-pwr-out-W)]}
   (BrakeComp. id min-pwr-out-W input log)))

(defrecord EnergyStorageComp
  [id max-capacity-J capacity-J log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (let [pwr-in-ach-W 0.0
              pwr-out-ach-W (bound pwr-out-req-W
                                   (max-power-out-W c dt-s)
                                   (min-power-out-W c dt-s))
              pwr-loss-ach-W 0.0
              e-stored-J (* -1.0 pwr-out-ach-W dt-s)
              new-c (assoc c
                      :log (update-log
                            (safe-get c :log)
                            dt-s
                            pwr-out-ach-W
                            pwr-in-ach-W
                            pwr-loss-ach-W
                            e-stored-J)
                      :capacity-J (+ (safe-get c :capacity-J)
                                     e-stored-J))]
          {:comp new-c
           :pwr-out-W pwr-out-ach-W}))
  (max-power-out-W [c dt-s] (/ capacity-J dt-s))
  (min-power-out-W [c dt-s] (/ (- capacity-J max-capacity-J) dt-s)))

(defn make-energy-storage-component
  "(id:Keyword * max-capacity-J:Real * capacity-J:Real) -> EnergyStorageComp
  |(id:Keyword * max-capacity-J:Real * capacity-J:Real * log:Log)
  -> EnergyStorageComp
  Create an energy storage component and return it."
  ([id max-capacity-J capacity-J]
   (make-energy-storage-component id max-capacity-J capacity-J (make-log)))
  ([id max-capacity-J capacity-J log]
   {:pre [(number? max-capacity-J) (number? capacity-J)]}
   (EnergyStorageComp. id max-capacity-J capacity-J log)))

(defn pwr-in-pwr-out-in-request-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real) -> pwr-in-req-W:Real
  Determine the power request INTO a power in/power out component."
  [c pwr-out-req-W dt-s]
  (let [outs-W (safe-get c :pwr-outs-W)
        ins-W (safe-get c :pwr-ins-W)
        interp-model (interp/mk-model outs-W ins-W)]
    (interp/with-edge-extension interp/linear-1d
                                interp-model
                                pwr-out-req-W)))

(defn pwr-in-pwr-out-out-achieved-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real * pwr-in-ach-W:Real)
  -> pwr-out-ach-W:Real
  Attempt to satisfy the power out achieved. The brake component
  will absorb negative power NOT already absorbed by the powertrain
  (i.e., negative value of pwr-in-ach-W) to the extent of min-pwr-out-W.
  Thus the total negative power absorption (i.e., braking) of the powertrain
  will be (+ min-pwr-out-W pwr-in-ach-W) when both values are negative."
  [c pwr-out-req-W dt-s pwr-in-ach-W]
  (let [outs-W (safe-get c :pwr-outs-W)
        ins-W (safe-get c :pwr-ins-W)
        interp-model (interp/mk-model ins-W outs-W)]
    (interp/with-edge-extension interp/linear-1d
                                interp-model
                                pwr-in-ach-W)))

(defrecord PowerInPowerOutComp
  [id pwr-ins-W pwr-outs-W input log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (generic-step c
                      pwr-out-req-W
                      dt-s
                      pwr-in-pwr-out-in-request-W
                      pwr-in-pwr-out-out-achieved-W))
  (max-power-out-W
   [c dt-s]
   (let [max-pwr-in-upstream (max-power-out-W (safe-get c :input) dt-s)
         max-pwr-out-upstream
         (pwr-in-pwr-out-out-achieved-W c
                                        max-pwr-in-upstream
                                        dt-s
                                        max-pwr-in-upstream)]
     (min (last (safe-get c :pwr-outs-W)) max-pwr-out-upstream)))
  (min-power-out-W
   [c dt-s]
   (let [min-pwr-in-upstream (min-power-out-W (safe-get c :input) dt-s)
         min-pwr-out-upstream
         (pwr-in-pwr-out-out-achieved-W c
                                        min-pwr-in-upstream
                                        dt-s
                                        min-pwr-in-upstream)]
     (max (first (safe-get c :pwr-outs-W)) min-pwr-out-upstream))))

(defn make-power-in-power-out-component
  "(id:Keyword * pwr-ins-W:[Real] * pwr-outs-W:[Real] * ins:[Comp])
  -> PowerInPowerOutComp
  (id:Keyword * pwr-ins-W:[Real] * pwr-outs-W:[Real] * ins:[Comp] * log:Log)
  -> PowerInPowerOutComp
  Create an energy storage component and return it."
  ([id pwr-ins-W pwr-outs-W input]
   (make-power-in-power-out-component
    id pwr-ins-W pwr-outs-W input (make-log)))
  ([id pwr-ins-W pwr-outs-W input log]
   {:pre [(every? number? pwr-ins-W)
          (every? number? pwr-outs-W)]}
   (PowerInPowerOutComp. id pwr-ins-W pwr-outs-W input log)))

(defrecord PowerCouplingComp
  [id power-split inputs log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (let [ins (safe-get c :inputs)
              pwr-in-maxs-W (map #(max-power-out-W % dt-s) ins)
              pwr-in-mins-W (map #(min-power-out-W % dt-s) ins)
              pwr-in-max-W (reduce + 0.0 pwr-in-maxs-W)
              pwr-in-min-W (reduce + 0.0 pwr-in-mins-W)
              pwr-in-req-W pwr-out-req-W
              pwr-in-req-lim-W (bound pwr-in-req-W
                                      pwr-in-max-W
                                      pwr-in-min-W)
              pwr-in-reqs-W ((safe-get c :power-split)
                             c
                             pwr-in-req-lim-W
                             pwr-in-maxs-W
                             pwr-in-mins-W
                             dt-s)
              states (map #(step %1 %2 dt-s) ins pwr-in-reqs-W)
              new-ins (mapv :comp states)
              pwr-in-achs-W (map :pwr-out-W states)
              pwr-in-ach-W (reduce + 0.0 pwr-in-achs-W)
              pwr-out-ach-W pwr-in-ach-W
              pwr-loss-ach-W 0.0
              new-c (-> (safe-get c :log)
                        (update-log
                         dt-s
                         pwr-out-ach-W
                         pwr-in-ach-W
                         pwr-loss-ach-W
                         0.0)
                        (->> (assoc c :log))
                        (assoc :inputs new-ins))]
          {:comp new-c
           :pwr-out-W pwr-out-ach-W}))
  (max-power-out-W
   [c dt-s]
   (reduce + 0.0
           (map #(max-power-out-W % dt-s)
                (safe-get c :inputs))))
  (min-power-out-W
   [c dt-s]
   (reduce + 0.0
           (map #(min-power-out-W % dt-s)
                (safe-get c :inputs)))))

(defn make-power-coupling
  "(id:Keyword * power-split:(Fn c:Comp * power-in-request-W:Real *
                                 maxs-W:[Real] * mins-W:[Real] *
                                 dtime-s:Real -> pwr-in-req-W:[Real])
  * inputs:[Comp]) -> PowerInPowerOutComp
  |(id:Keyword * power-split:(Fn c:Comp * power-in-request-W:Real *
                                 maxs-W:[Real] * mins-W:[Real] *
                                 dtime-s:Real -> pwr-in-req-W:[Real])
  * inputs:[Comp] * log:Log) -> PowerInPowerOutComp
  Create a power coupling component and return it."
  ([id power-split inputs]
   (make-power-coupling id power-split inputs (make-log)))
  ([id power-split inputs log]
   (PowerCouplingComp. id power-split inputs log)))

(defn const-load-in-request-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real) -> pwr-in-req-W:Real
  Calculates the power in requested (in Watts) with a constant
  loss (load) component"
  [c pwr-out-req-W dt-s]
  (+ (safe-get c :load-W) pwr-out-req-W))

(defn const-load-out-achieved-W
  "(c:Comp * pwr-out-req-W:Real * dt-s:Real * pwr-in-ach-W:Real)
  -> pwr-out-ach-W:Real
  Calculate the power out achieved given power in achieved."
  [c pwr-out-req-W dt-s pwr-in-ach-W]
  (let [load-W (Math/abs (- pwr-in-ach-W pwr-out-req-W))]
    (- pwr-in-ach-W load-W)))

(defrecord ConstLoadComp
  [id load-W input log]
  Comp
  (step [c pwr-out-req-W dt-s]
        (generic-step c
                      pwr-out-req-W
                      dt-s
                      const-load-in-request-W
                      const-load-out-achieved-W))
  (max-power-out-W
   [c dt-s]
   (let [max-pwr-in-upstream (max-power-out-W (safe-get c :input) dt-s)]
     (- max-pwr-in-upstream (safe-get c :load-W))))
  (min-power-out-W
   [c dt-s]
   (let [min-pwr-in-upstream (min-power-out-W (safe-get c :input) dt-s)]
     (- min-pwr-in-upstream (safe-get c :load-W)))))

(defn make-constant-load-component
  "(id:Keyword * load-W:Real * input:Comp) -> ConstLoadComp
  |(id:Keyword * load-W:Real * input:Comp * log:Log) -> ConstLoadComp
  Make constant load component."
  ([id load-W input]
   (make-constant-load-component id load-W input (make-log)))
  ([id load-W input log]
   {:pre [(number? load-W)]}
   (ConstLoadComp. id load-W input log)))

(defn find-comp
  "(c:Comp * id:Keyword) -> Comp|nil
  Finds a component (depth first search) with the given id"
  [c id]
  (loop [cs [c]]
    (if (empty? cs)
      nil
      (let [this-c (first cs)]
        (if (= id (safe-get this-c :id))
          this-c
          (recur (concat (if (contains? this-c :input)
                           [(safe-get this-c :input)]
                           (if (contains? this-c :inputs)
                             (safe-get this-c :inputs)
                             []))
                         (rest cs))))))))

(defn mk-sim-reducer
  "(fwd-sim:(Fn State Power-W ElapsedTime-s -> State)
   * bwd-sim:(Fn State State -> Power-W))
   -> {:powertrain Comp :states [State]}
  Create a function for use in performing powertrain simulation"
  [fwd-sim bwd-sim]
  (fn [state x2]
    (let [x1 (last (safe-get state :states))
          pwr-req-W (bwd-sim x1 x2)
          dt (- (safe-get x2 :time-s) (safe-get x1 :time-s))
          out (step (safe-get state :powertrain) pwr-req-W dt)
          pwr-ach-W (safe-get out :pwr-out-W)]
      (-> state
          (assoc :powertrain (safe-get out :comp))
          (update-in [:states]
                     conj
                     (if (< (Math/abs (- pwr-ach-W pwr-req-W)) 1e-6)
                       x2
                       (let [fwd-out (fwd-sim x1 pwr-ach-W dt)]
                         (ld/mk-state (safe-get fwd-out :time-s)
                                      (safe-get fwd-out :speed-m--s)
                                      (safe-get fwd-out :elevation-m)
                                      (safe-get x2 :flags)
                                      (safe-get x2 :step-wise)))))))))

(defn simulate
  "(loads:[Load] * veh:Vehicle * env:Environment * duty-cycle:DutyCycle
  * pt:Comp) -> {:powertrain Comp :states [StatePoint]}
  Simulate the given powertrain over the requested duty cycle represented
  by the state points. Return the state points achieved."
  [loads veh env duty-cycle pt]
  (let [fwd-sim (ld/mk-forward-calculator loads veh env)
        bwd-sim (ld/mk-backward-calculator loads veh env)
        xs (dc/to-statepoints duty-cycle)
        reducer (mk-sim-reducer fwd-sim bwd-sim)]
    (reduce reducer
            {:powertrain pt
             :states [(first xs)]}
            (rest xs))))
