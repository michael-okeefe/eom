;; Copyright 2013 Michael Patrick O'Keefe and ImagineMade LLC.
;; All Rights Reserved.
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;; You must not remove this notice, or any other, from this software.
(ns eom.utils)

;; the following from Prismatic plumbing 0.1.1
;; Prismatic plumbing is Copyright (C) 2013 Prismatic.
;; Distributed under the Eclipse Public License, the same as Clojure.
;; https://github.com/Prismatic/plumbing/blob/master/project.clj
(defmacro lazy-get
  "Like get but lazy about default"
  [m k d]
  `(if-let [pair# (find ~m ~k)]
     (val pair#)
     ~d))

;; the following from Prismatic plumbing 0.1.1
;; Prismatic plumbing is Copyright (C) 2013 Prismatic.
;; Distributed under the Eclipse Public License, the same as Clojure.
;; https://github.com/Prismatic/plumbing/blob/master/project.clj
(defn safe-get
  "Like get but throw an exception if not found"
  [m k]
  (lazy-get
   m k (throw
        (IllegalArgumentException.
         (format "Key %s not found in %s" k (mapv key m))))))

;; the following from Prismatic plumbing 0.1.1
;; Prismatic plumbing is Copyright (C) 2013 Prismatic.
;; Distributed under the Eclipse Public License, the same as Clojure.
;; https://github.com/Prismatic/plumbing/blob/master/project.clj
(defn safe-get-in
  "Like get-in but throws exception if not found"
  [m ks]
  (if (seq ks)
    (recur (safe-get m (first ks)) (next ks))
    m))
