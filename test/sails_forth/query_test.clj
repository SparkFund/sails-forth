(ns sails-forth.query-test
  (:require [clj-time.core :as time]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer :all]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :refer :all]
            [sails-forth.query :refer :all]
            [sails-forth.test :as test]))

(deftest test-soql-value
  (is (= "''" (soql-value "")))
  (is (= "'test'" (soql-value "test")))
  (is (= "'escape\\\\backslashes'" (soql-value "escape\\backslashes")))
  (is (= "'escape\\nnewlines'" (soql-value "escape\nnewlines")))
  (is (= "'escape\\'quotes'" (soql-value "escape'quotes"))))

;;; NOTE these tests are not guaranteed to work on arbitrary salesforce dbs

(deftest ^:integration test-soql-query
  (let [client (sf/build-http-client (test/load-config))]
    (is (= "SELECT Id FROM Opportunity"
           (soql-query client :opportunity [(sf/resolve-attr-path client :opportunity [:id])] [])))))

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
