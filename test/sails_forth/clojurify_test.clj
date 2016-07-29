(ns sails-forth.clojurify-test
  (:require [clj-time.core :as time]
            [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :refer :all]
            [sails-forth.test :as test]))

;;; NOTE these tests are not guaranteed to work on arbitrary salesforce dbs

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
  (testing "percent"
    (is (= 0.0618M (parse-value {:type "percent"
                                 :scale 2
                                 :precision 4}
                                6.18M))))
  (testing "int"
    (is (= 500 (parse-value {:type "int"} 500M))))
  (testing "other"
    (is (= "foo" (parse-value {} "foo")))))

(deftest test-render-value
  (testing "datetime"
    (is (= (render-value {:type "datetime"}
                         (time/date-time 2015 1 1 12 30 15 500))
           "2015-01-01T12:30:15.500Z"))
    (is (= (render-value {:type "datetime"}
                         "2015-01-01T12:30:15.500Z")
           "2015-01-01T12:30:15.500Z")))
  (testing "date"
    (is (= (render-value {:type "date"}
                         (time/local-date 2015 2 1))
           "2015-02-01")))
  (testing "percent"
    (is (= (render-value {:type "percent"
                          :scale 2
                          :precision 4}
                         0.0618M)
           6.18M)))
  (testing "other"
    (is (= "foo" (parse-value {} "foo")))))

(deftest ^:integration test-get-field-description
  (let [client (sf/build-http-client (test/load-config))]
    (is (get-field-description client :opportunity :id))
    (is (get-field-description client :payment :opportunity))))

(deftest ^:integration test-resolve-attr-path
  (let [client (sf/build-http-client (test/load-config))]
    (is (resolve-attr-path client :opportunity [:id]))
    (is (resolve-attr-path client :opportunity [:counterparty-account :id]))
    (is (resolve-attr-path client :opportunity [:counterparty-account :recordtype :name]))
    (is (resolve-attr-path client :payment [:opportunity :id]))
    (is (resolve-attr-path client :payment [:project-schedule :id]))))
