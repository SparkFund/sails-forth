(ns sails-forth.query-test
  (:require [clj-time.core :as time]
            [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [sails-forth :as sf]
            [sails-forth-test :as sft]
            [sails-forth.query :refer :all]))

;;; NOTE these tests are not guaranteed to work on arbitrary salesforce dbs

(deftest test-types
  #_(t/check-ns 'sails-forth.query))

(deftest test-parse-value
  (testing "datetime"
    (is (= (time/date-time 2015 1 1 12 30 15 500)
           (parse-value {:type "datetime"}
                        "2015-01-01T12:30:15.500Z"))))
  (testing "date"
    (is (= (time/local-date 2015 2 1)
           (parse-value {:type "date"}
                        "2015-02-01"))))
  (testing "double"
    (is (= 500 (parse-value {:type "double"
                             :scale 0
                             :precision 18}
                            500M)))
    (is (= 500N (parse-value {:type "double"
                              :scale 0
                              :precision 19}
                             500M)))
    (is (= 500M (parse-value {:type "double"
                               :scale 2
                              :precision 18}
                             500M))))
  (testing "int"
    (is (= 500 (parse-value {:type "int"} 500M))))
  (testing "other"
    (is (= "foo" (parse-value {} "foo")))))

(deftest test-get-field-description
  (let [client (sf/build-client! (sft/load-config))]
    (is (get-field-description client :opportunity :id))
    (is (get-field-description client :payment :opportunity))))

(deftest test-resolve-attr-path
  (let [client (sf/build-client! (sft/load-config))]
    (is (resolve-attr-path client :opportunity [:id]))
    (is (resolve-attr-path client :opportunity [:counterparty-account :id]))
    (is (resolve-attr-path client :opportunity [:counterparty-account :recordtype :name]))
    (is (resolve-attr-path client :payment [:opportunity :id]))
    (is (resolve-attr-path client :payment [:project-schedule :id]))))

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
