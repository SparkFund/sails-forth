(ns sails-forth.graphql-test
  (:require [clojure.test :refer :all]
            [sails-forth.graphql :refer :all]))

(deftest test-resolve-soql-fields
  (let [sf-schema {"Opportunity"
                   {:fields
                    [{:name "Id"
                      :type "id"}
                     {:name "AccountId"
                      :type "reference"
                      :referenceTo ["Account"]
                      :relationshipName "Account"}
                     {:name "Contractor_primary__c"
                      :type "reference"
                      :referenceTo ["Account"]
                      :relationshipName "Contractor_primary__r"}]}
                   "Account"
                   {:fields
                    [{:name "Id"
                      :type "id"}]}}]
    (is (= #{"Id"
             "Account.Id"
             "Contractor_primary__r.Id"}
           (resolve-soql-fields sf-schema "Opportunity"
                                {:Opportunity/Id nil
                                 :Opportunity/AccountId
                                 {:selections {:Account/Id nil}}
                                 :Opportunity/Contractor_primary__c
                                 {:selections {:Account/Id nil}}})))))
