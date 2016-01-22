(ns sails-forth.query-test
  (:require [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [sails-forth.query :refer :all]))

(deftest test-types
  (t/check-ns 'sails-forth.query))
