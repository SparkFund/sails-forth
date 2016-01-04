(ns user
  (:require [clojure.core.typed :as t]
            [sails-forth :as sf]
            [sails-forth-test]))

(def config
  (sails-forth-test/load-config))

(def client
  (when config
    (sf/build-client! config)))
