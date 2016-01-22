(ns sails-forth.repl
  (:require [clojure.core.typed :as t]
            [clojure.edn :as edn]
            [sails-forth :as sf]
            [sails-forth.query :as sq]))

(defn build-client!
  [path]
  (-> path slurp edn/read-string sf/build-client!))
