# Economy of Motion (TM) Vehicle Library

Economy of Motion is a library for conducting vehicle energy-use
and (eventually) economic analyses using the [Clojure](http://clojure.org)
Programming Language.

## Project Goals

To be a full featured library for constructing, editing/manipulating,
and/or conducting analysis on:

* vehicle driving profiles
* vehicle powertrains
* vehicle life-cycle costs
* fleet energy usage and economics
* transportation systems and networks including emissions and congestion

## Project Maturity

This software is still under heavy development with several planned
changes to the core data structures and modules. As such, use with
caution.

Development on Economy of Motion began in 2011 and the analysis
capabilities for driving profiles are both tested and mature although we
intent to rework the core dutycycle structure. Several of the other
features for this library have not been implemented yet and a
user-facing interface is still lacking.

Next steps include the development of a vehicle power-flow energy
model, an effort-flow semi-empirical model, fleet models, economics,
and drive cycle generation based on road networks and trip generation.

## Supported Clojure Versions

Development for Economy of Motion is conducted under Clojure 1.5.1.
When the project reaches a higher level of maturity, we intend to more
thoroughly investigate support for earlier versions of Clojure.

## Getting Started

The file presented at the Clojure Conj 2013 is in the eom.demo.conj2013
namespace. That may be a good place to start exploring.

### To run the code:

To be developed

## Community

To be developed. For now, please use Github's system for reporting bugs,
feature enhancements, and the like. We intend to setup a discussion list
at a later time for specific discussions.

## Transportation Energy Primer

The United States consumes more petroleum for transportation than it
produces -- in fact, the most recent statistics show we use 161% of
our total petroleum production for transportation. The remainder of
our transportation petroleum comes from foreign imports. Petroleum
comprises 93% of our total transportation fuel energy usage, with cars
and light trucks (totaling over 230 million vehicles in the US) using
59% of our total transportation energy, medium duty trucks account for
5%, and heavy duty trucks for 17% (TEDB 2012).

Compared to other energy end use sectors, transportation accounts for
28% of the US total energy usage (ibid). Transportation is a critical
part of our economy and major disruption to our transportation system
would have a large economic impact.

Across the globe, there are an estimated 850 million vehicles. This may
sound like a large number but yet it only represents about one vehicle
for every eight of us. Is the automobile as presently conceived a
sustainable proposition? Are their better ways to meet this need?

Bottom line: we use a lot of energy, mostly petroleum, to power our
transportation fleet here in the United States. With this much energy
being used, much of it from foreign sources, it is important to study
and understand how and why we use this energy and how choices to
transportation policy, technology, and habits can effect this bottom
line.

## Rationale for Another Vehicle Simulator

Several vehicle simulation programs have been created over the years
to study vehicle energy usage. I have had the privilege to help build
three vehicle/transportation simulators prior to this one and have
used many of the others. In the 1990s when there were serious
questions from policy makers on the potential role of hybrid electric
vehicles, many of these simulators played critical roles in answering
questions and were used by academics, policy makers, and vehicle
original equipment manufacturers (OEMs) and their suppliers.

Several simulators still exist and some continue under active
development. To these simulators, Economy of Motion adds the
following:

* A standard open-source license for the source-code. We have chosen to use the Mozilla Public License Version 2.0 with this code. The MPL v2.0 keeps the spirit of the Eclipse Public License while offering the ability to compose with GPL-based tools. This offers the greatest end-user composibility options.
* Hosted on a freely available feature-rich run-time system: the Java Virtual Machine (JVM)
* A library approach. Many (all?) other vehicle simulation tools present the end-user with an application. There is nothing wrong with applications per se, but it can be difficult to put together a working application that has the flexibility to accommodate and anticipate all potential analyses one wishes to conduct. We feel it is a stronger proposition to start out as a library instead.
* Support for all aspects of transportation modeling. We hope to eventuall have enough material in the library to support vehicle-level, fleet-level, and up to regional and national transportation analyses. Many (all?) other simulators are strictly in one paradigm: vehicle level with fixed solution stragegy or congestion model.

I think it's especially interesting to note that there is no free and
open-source vehicle simulators available on freely available runtime
systems. Economy of Motion hopes to change that and, at the same time,
set the bar for vehicle simulators that come afterwards.

## References

(TEDB 2012) Davis, S.; Diegel, S.; Boundy, R. (2012). "Transportation
Energy Data Book". 31st Edition. Prepared for the Vehicle Technologies
Program of the Office of Energy Efficiency and Renewable Energy at the
U.S. Department of Energy. Available on-line: http://cta.ornl.gov/data

## Trademarks

"Economy of Motion" is a trademark of ImagineMade LLC. This License does
not grant any rights in the trademarks, service marks, or logos of any
Contributor. See LICENSE for details.

## Copyright and License (see license.html for further detail)

Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC. All rights reserved.

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Note: The Mozilla Public License version 2.0 was selected for this code
base as it is similar to the Eclipse Public License in spirit while
providing for enhanced unity and compatibility with other licenses
(see http://opensource.org/node/594).
