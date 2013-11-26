;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; This module based on Michael Fogus and Chris Houser "Joy of Clojure" first
;; edition, ;; chapter 13, listing 13.2 and 13.3. Simplified to not use double
;; unit lookup. All units must be directly specified in terms of the base unit.
;; Note: also referenced minderbinder project src/minderbinder/core.clj by
;; Michael Fogus ;; available at: https://github.com/fogus/minderbinder (under
;; EPL license)
;;
;; unit-multiplier, build-conversion-map, and def-units-of based on work
;; of Michael Fogus in the above references. Names modified and functions
;; slightly changed for optimal use in EOM.
(ns eom.unit-conversion
  (:require [eom.numeric :as N])
  (:refer-clojure :exclude [time]))

; unit-multiplier -- returns the unit-conversion multiplier to take the given
;                    unit to the base unit
; u: keyword * units: map * history: seq -> unit-multiplier: number
; note: history is a vector of previous conversions
(defn- unit-multiplier
  [u units history]
  (if (some #{u} history)
    (throw (Exception. (str "Cyclic dependency: " u " in " history))))
  (let [conversion (u units)
        new-history (conj history u)]
    (if (nil? conversion)
      (throw (Exception. (str "Undefined unit " u
                              "; no conversion for " u)))
      (cond (vector? conversion) (let [[other-conv other-u] conversion]
                                   (try
                                     (* other-conv
                                        (unit-multiplier other-u
                                                         units
                                                         new-history))
                                     (catch ArithmeticException e
                                       (throw (ArithmeticException.
                                               (str (.getCause e)
                                                    " in " conversion))))))
            (keyword? conversion) (unit-multiplier conversion
                                                   units
                                                   new-history)
            :otherwise (eval conversion)))))

; build-conversion-map -- creates the unit conversion map but accounts for nice
;                         features such as using sets to specify multiple units
; base-unit: keyword * unit-pairs: seq -> map: keyword/unit-conversion
; note: unit-conversion can be
;        1. a conversion:number
;        2. a vector of [conversion:number in-terms-of-another-unit:keyword]
;        3. a unit-alias:keyword
;        4. or a set-of-unit-aliases:set of keywords
(defn- build-conversion-map
  [base-unit unit-pairs]
  (into `{~base-unit 1}
        (reduce concat
                (for [[k v] (partition 2 unit-pairs)]
                  (if (set? v)
                    (map vec (partition 2 (interleave v (repeat k))))
                    [[k v]])))))

; def-units-of -- macro to automate writing unit-of-XXX functions
; measure: keyword * base-unit: keyword * conversions: keyword number pairs -> nil
(defmacro def-units-of
  [measure base-unit desc & conversions]
  (let [quantity (gensym)
        unit (gensym)
        base-unit-1 (gensym)
        base-unit-2 (gensym)
        conversion-table (build-conversion-map base-unit conversions)
        conversion-macro-name (symbol measure)
        parse-fn-name (symbol (str "parse-" measure))
        convert-fn-name (symbol (str "convert-" measure))
        conversion-table-name (symbol (str measure "-conversion-table"))]
    `(do
       (defmacro ~conversion-macro-name
         "[number keyword] -> number
         Convert the given quantity (number) in the given units (given
         by keyword, e.g., :meter for a unit of length) and convert
         it to the base unit."
         [~quantity ~unit]
         `(* ~~quantity
             ~(case ~unit
                    ~@(mapcat
                       (fn [[u# & r#]]
                         `[~u# ~(unit-multiplier u# conversion-table [])])
                       conversion-table))))

       (defn ~parse-fn-name
         "[(number keyword)+] -> number
         Parse a series of number keyword pairs into
         the base unit for this unit of measure"
         [descr#]
         (let [conv# ~conversion-table]
           (reduce +
                   (map #(let [[mag# u#] %
                               r# (get conv# u#)]
                           (cond (keyword? r#) (~parse-fn-name
                                                [mag# r#])
                                 (vector?  r#) (* mag#
                                                  (~parse-fn-name
                                                   r#))
                                 :default (* mag# r#)))
                        (partition 2 descr#)))))

       (def ~conversion-table-name ~conversion-table)

       (defn ~convert-fn-name
         ~(str "[number [number keyword] [number keyword]] -> number
               For the " measure " unit of measure, convert the given quantity
               (a number), from unit specification one (consisting of a vector
               of quantity and keyword indicating the unit -- for example :m
               or :meter) to unit specification two.")
         [qty# u1# u2#]
         (let [~base-unit-1 (~parse-fn-name u1#)
               ~base-unit-2 (~parse-fn-name u2#)]
           (* qty# (/ ~base-unit-1 ~base-unit-2))))

       ~conversion-table)))

(def-units-of length :m "Length"
  :m #{:meter :meters}
  :km 1000
  :km #{:kilometer :kilometers}
  :dm 1/10
  :dm #{:decimeter :decimeters}
  :cm 1/100
  :cm #{:centimeter :centimeters}
  :mm 1/1000
  :mm #{:millimeter :millimeters}
  :ft [12 :in]
  :ft #{:foot :feet}
  :in [254/100 :cm]
  :in #{:inch :inches}
  :mi [5280 :ft]
  :mi #{:mile :miles}
  :yd [3 :ft]
  :yd #{:yard :yards}
  :surveyfoot 1200/3937
  :surveyfoot #{:surveyfeet :surveyft})

(def-units-of area :m2 "Area"
  :surveyfoot2 (N/square (length 1 :surveyfoot))
  :surveyfoot2 #{:surveyft2}
  :acre [43560 :surveyft2]
  :ft2 (N/square (length 1 :foot)))

(def-units-of volume :m3 "Volume"
              :cm3 (N/cube (length 1 :cm))
              :cc :cm3
              :L [1000 :cc]
              :L #{:liter :liters}
              :in3 (N/cube (length 1 :in))
              :in3 #{:cubic-inches :inches3}
              :gal [231 :in3]
              :gal #{:gallon :gallons})

(def-units-of mass :kg "Mass"
              :kilogram 1
              :lbs 0.45359237
              :lbs #{:pound :pounds :pounds-mass :lbs-mass :lbm}
              :g 1/1000)

(def-units-of absolute-temperature :K "Absolute Temperature"
              :K #{:kelvin}
              :R [5/9 :K]
              :R #{:rankine})

(def-units-of temperature-difference :K "Temperature Difference"
              :K #{:kelvin :centigrade :celsius :C}
              :R [5/9 :K]
              :R #{:rankine :F :fahrenheit})

(def-units-of time :s "Time"
              :s #{:second :seconds :sec :secs}
              :hr 3600
              :hr #{:hour}
              :min 60
              :min #{:minute :minutes}
              :day 86400
              :day #{:days}
              :wk [7 :days]
              :wk #{:week :weeks}
              :yr [365.242 :days]
              :yr #{:year :years})

(def-units-of speed :m--s "Speed"
              :m--s #{:meters-per-second :meter-per-second :mps}
              :mph 0.44704
              :mph #{:miles-per-hour :mile-per-hour :mi--hr}
              :kph 0.277778
              :kph #{:km--hr})

(def-units-of power :W "Power"
              :W #{:watt :watts :Watt :Watts}
              :kW 1000.0
              :kW #{:kilowatts :kilowatt}
              :hp 745.699872
              :hp #{:horsepower}
              :MW 1e6
              :MW #{:megawatt :megawatts})

(def-units-of energy :J "Energy"
              :J #{:joule :Joule}
              :kJ 1000.0
              :Wh 3600.0
              :kWh 3.6e6
              :Wh #{:watthours :watt-hours}
              :kWh #{:kilowatt-hours})

; old defs
; (def slugs__ft3-to-kg__m3 515.379)

; http://www.energy.rochester.edu/units/conversions.pdf
; see http://futureboy.us/frinkdata/units.txt
; (def Btu-to-MJ 1.055)

; (def Million-Btu-to-MJ 1055.0)

; (def liters-to-US-gallons 0.264172052)

; (def US-gallons-to-liters 3.78541178)
