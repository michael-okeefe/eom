;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; # High-Level Powertrain (HLPT) Models
;;
;; This namespace contains high-level (i.e., low detail) models that allow
;; one to obtain vehicle fuel consumption from a minimal amount of information.
;; Assumptions include fixed efficiency for motoring and regenerative braking
;; (if any) as well as motoring and regen efficiencies that are a function of
;; roadload power.
;;
;; These models are meant to be used for feasibility study and for large
;; fleet analyses where the ability to get a rough estimate of the fuel
;; consumption for many different vehicles quickly is important.
(ns eom.hlpt
  (:require [eom.vehicle :as veh]
            [eom.dutycycle :as cyc]
            [eom.interp :as interp]
            [eom.fuels :as fuels]
            [eom.resources :as rsrc]))

(defn mk-const-eff-fc-fn
  "float float -> (float float -> float)
  * pt-eff -- powertrain efficiency for tractive effort; float over (0 1]
  * regen-eff -- regenerative braking efficiency for regen events;
                 float over [0 1]
  Returns a constant efficiency fuel consumption function that takes in
  roadload (J) and the duration of the time step (in seconds) and returns
  the fuel energy consumed (or recovered) in J. Note that the function
  constructed here does not take advantage of the time-step duration
  information."
  [pt-eff regen-eff]
  {:pre [(and (> pt-eff 0.0) (<= pt-eff 1.0)
              (>= regen-eff 0.0) (<= regen-eff 1.0))]}
  (fn [roadload-J _]
    (if (>= roadload-J 0.0)
      (/ roadload-J pt-eff)
      (* roadload-J regen-eff))))

(defn mk-eff-by-power-fc-fn
  "seq-of-float seq-of-float -> (float float -> float)
  * roadloads-W -- a vector of increasing roadload powers which may include
                   negative values indicating regen
  * fcs-W       -- the corresponding fuel consumptions for the given
                   roadload powers
  Return a function that takes roadload for the given time-step
  (in J) and the time-step duration (in seconds) and returns the
  fuel consumed (in J) for that time-step."
  [roadloads-W fcs-W]
  {:pre [(= (count roadloads-W) (count fcs-W))]}
  (fn [roadload-J time-s]
    (let [roadload-W (/ roadload-J time-s)
          model (interp/mk-model roadloads-W fcs-W)
          fc-W (interp/with-edge-extension interp/linear-1d model roadload-W)]
      (* time-s fc-W))))

(defn load-fc-by-roadload-fn
  "Loads data to form a fuel-consumption (W) function by roadload power (W)"
  [path]
  (rsrc/load-edn path
                 {'eom/roadload-fc-data
                 (fn [data]
                   (let [rl-fcs (partition 2 data)
                         rls (map first rl-fcs)
                         fcs (map second rl-fcs)]
                     (mk-eff-by-power-fc-fn rls fcs)))}))

(defn fuel-consumptions-inf-ess-J
  "veh dc (float float -> float) -> seq-of-float
  * veh -- the vehicle data
  * dc  -- duty cycle data
  * fc-fn -- the fuel-consumption function which must take
             in the roadload required (J) and the duration of the time
             step (s) and return the fuel energy (J) consumed (or recovered).
  Returns the fuel energies required per time-step (J) assuming
  a single infinite energy storage system capable of accepting
  and providing for the entire roadload."
  [veh dc fc-fn]
  (map fc-fn (veh/roadload-energies-J veh dc) (cyc/time-steps-s dc)))

(defn fuel-consumptions-limited-ess-J
  "veh dc limits -> seq-of-float
  * veh -- the given vehicle
  * dc  -- duty cycle
  * limits -- the limits of the given powertrain which is a map
              with the following possible keyword keys:
    - :fc-fn -- fuel consumption function; a fn taking roadload (J)
                and time-step duration (s) and return the fuel used
                (during motoring) or captured (during regen) in J
    - :ess-cap-J -- usable capacity of the energy storage system (J)
                    on the vehicle -- a float over range [0 inf)
    - :init-soe  -- the state of energy at duty cycle start for
                    the energy storage system (ess); float with range
                    from [0 1]
    - :min-pwr-W -- the minimum power (in W) during regen events that can be
                    uptake (a negative number). The lower the number,
                    the larger the amount of regenerative braking energy
                    that can be absorbed.
    - :max-pwr-W -- the maximum motoring power output (W) that can be achieved
  Returns a map with the following keys:
    :fcs-J -- sequence of floats of the fuel energies (J) required per
              time-step assuming the powertrain has the given limits in power
              and capacity.
    :ess-soes-J -- the state of energy of energy storage system per sample time
                   of the driving cycle in J."
  [veh dc {:keys [fc-fn ess-cap-J init-soe min-pwr-W max-pwr-W]}]
  (let [roadloads-J (veh/roadload-energies-J veh dc)
        times-s (cyc/time-steps-s dc)]
    (reduce (fn [{:keys [ess-soes-J fcs-J] :as state} [roadload-J dt-s]]
              (let [clipped-roadload-W (-> (/ roadload-J dt-s)
                                           (#(if (> % max-pwr-W) max-pwr-W %))
                                           (#(if (< % min-pwr-W) min-pwr-W %)))
                    fc-J (fc-fn (* clipped-roadload-W dt-s) dt-s)
                    last-ess-soe-J (last ess-soes-J)
                    next-ess-soe-J (-> (- last-ess-soe-J fc-J)
                                       (#(if (> % ess-cap-J) ess-cap-J %))
                                       (#(if (< % 0.0) 0.0 %)))
                    clipped-fc-J (- last-ess-soe-J next-ess-soe-J)
                    state-a (update-in state [:fcs-J] conj clipped-fc-J)]
                (update-in state-a [:ess-soes-J] conj next-ess-soe-J)))
            {:ess-soes-J [(* init-soe ess-cap-J)]
             :fcs-J []}
            (map vector roadloads-J times-s))))

(defn veh-fuel-econ
  "A simple vehicle fuel economy calculator
  - veh :: vehicle
  - dc :: duty cycle
  - pt-eff :: powertrain efficiency (0.0 < eff <= 1.0)
  - regen-eff (optional) :: regenerative braking efficiency (0.0 <= eff <= 1.0)
                            defaults to 0.0
  - fuel (optional) :: the fuel to use -- defaults to Gasoline
  Returns map<keyword, float> :: {:mpg # :L--100km #}"
  ([veh dc pt-eff]
   (veh-fuel-econ veh dc pt-eff 0.0 fuels/gasoline))
  ([veh dc pt-eff regen-eff]
   (veh-fuel-econ veh dc pt-eff regen-eff fuels/gasoline))
  ([veh dc pt-eff regen-eff fuel]
   (let [fcs (fuel-consumptions-inf-ess-J
              veh dc (mk-const-eff-fc-fn pt-eff regen-eff))
         fc (reduce + fcs)
         D (cyc/distance-m dc)
         mpg (fuels/as-mpg fc D fuel)
         L--100km (fuels/as-L--100km fc D fuel)]
     {:mpg mpg :L--100km L--100km})))
