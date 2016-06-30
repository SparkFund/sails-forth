(ns sails-forth.query-test
  (:require [clj-time.core :as time]
            [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [sails-forth :as sf]
            [sails-forth-test :as sft]
            [sails-forth.query :refer :all]
            [sails-forth.clojurify :refer :all]))

;;; NOTE these tests are not guaranteed to work on arbitrary salesforce dbs

(deftest test-types
  #_(t/check-ns 'sails-forth.query))

(deftest test-soql-query
  (let [client (sf/build-client! (sft/load-config))]
    (is (= "SELECT Id FROM Opportunity"
           (soql-query client :opportunity [(resolve-attr-path client :opportunity [:id])] [])))))

(deftest test-query
  (let [client (sf/build-client! (sft/load-config))]
    (is (query client {:find [:payment
                              :id
                              [:opportunity :id]
                              [:project-schedule :id]]}))
    (is (query client {:find [:payment
                              :id
                              [:opportunity :name]]
                       :where [[:= [:payment :opportunity :name] "Test Opportunity"]]}))))
