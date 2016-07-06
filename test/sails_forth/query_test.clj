(ns sails-forth.query-test
  (:require [clj-time.core :as time]
            [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :refer :all]
            [sails-forth.query :refer :all]
            [sails-forth.test :as test]))

;;; NOTE these tests are not guaranteed to work on arbitrary salesforce dbs

(deftest ^:integration test-soql-query
  (let [client (sf/build-http-client (test/load-config))]
    (is (= "SELECT Id FROM Opportunity"
           (soql-query client :opportunity [(resolve-attr-path client :opportunity [:id])] [])))))

(deftest ^:integration test-query
  (let [client (sf/build-http-client (test/load-config))]
    (is (query client {:find [:payment
                              :id
                              [:opportunity :id]
                              [:project-schedule :id]]}))
    (is (query client {:find [:payment
                              :id
                              [:opportunity :name]]
                       :where [[:= [:payment :opportunity :name] "Test Opportunity"]]}))))
