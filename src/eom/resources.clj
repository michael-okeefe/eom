;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
;;
;; # eom.resources
;;
;; This module provides functions to work with loading and saving resources
;; from various "resource repositories" which
;; are abstractions around a data store:
;; * database connection
;; * directories
;; * remote version controlled repositories
;;
;; In the context of Economy of Motion, these remote data stores contain
;; data representing driving cycles, internal combustion engines,
;; electric machines, power electronics, battery packs, material properties,
;; geometry, etc.
(ns eom.resources
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.io.PushbackReader]))

(defn load-edn
  "Loads an EDN file using the given readers"
  [path readers]
  (let [stream (java.io.PushbackReader. (io/reader path))]
    (edn/read {:eof :NOT-FOUND :readers readers} stream)))

; data-id :: [repo-uri ["data identifiers" ...]]
; listing :: seq-of-seq-of-string where string is a "data identifier"
;
; interface
; fetch :: data-id -> data | listing
