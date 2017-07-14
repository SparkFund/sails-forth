(ns sails-forth.repl
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :as sc]
            [sails-forth.query :as sq]
            [sails-forth.spec :as spec]
            [sails-forth.update :as su]))

(defn build-client!
  [path]
  (-> path slurp edn/read-string sf/build-http-client))
