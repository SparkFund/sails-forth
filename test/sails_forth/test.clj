(ns sails-forth.test
  (:require [clojure.edn :as edn]))

(defn load-config
  []
  (some-> (try (slurp "test/config.edn") (catch Exception _))
          edn/read-string))
