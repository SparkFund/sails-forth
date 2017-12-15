(ns sails-forth.datomic-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer :all]
            [sails-forth.client :as sf]
            [sails-forth.datomic :refer :all]))

(deftest test-assert-query
  (let [schema (edn/read-string (slurp "test/schema.edn"))
        client (sf/build-memory-client schema)
        user-id (sf/create! client "User" {"Name"  "Donald"})
        _ (sf/create! client "Payment__c" {"Amount__c" 5
                                           "CreatedById" user-id})
        txns (assert-query client "sf" {:find [:payment :id
                                               [:createdby :id :name]]})]
    (is (= 3 (count txns)))
    (is (= "Donald" (get-in txns [2 0
                                  :sf.object.payment/createdby
                                  :sf.object.user/name])))))
