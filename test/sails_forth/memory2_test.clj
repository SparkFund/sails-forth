(ns sails-forth.memory2-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [sails-forth.client :as sf]
            [sails-forth.memory2 :refer :all]
            [sails-forth.query :as sq])
  (:refer-clojure :exclude [update list]))

(deftest test-memory-client
  (let [schema (edn/read-string (slurp "test/schema.edn"))
        client (sf/build-memory-client schema {"custom/apex/TestEndpoint" (fn [c inputs] inputs)})]
    (testing "schema"
      (is (= #{"Payment__c" "User"}
             (set (map :name (:sobjects (sf/objects! client))))))
      (is (every? (set (map :name (:fields (sf/describe! client "Payment__c"))))
                  #{"Id" "Amount__c"})))
    (testing "crud"
      (let [id (sf/create! client "Payment__c" {"Amount__c" 5})]
        (is id)
        (is (= [{"Id" id "Amount__c" 5}] (sf/list! client "Payment__c")))
        (is (sf/update! client "Payment__c" id {"Amount__c" 10}))
        (is (= [{"Id" id "Amount__c" 10}] (sf/list! client "Payment__c")))
        (is (sf/delete! client "Payment__c" id))
        (is (not (seq (sf/list! client "Payment__c"))))))
    (testing "queries"
      (let [user-id (sf/create! client "User" {"Name"  "Donald"})]
        (sf/create! client "Payment__c" {"Amount__c" 5
                                         "CreatedById" user-id}))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}}]
             (sf/query! client (str "select Amount__c from Payment__c"))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}}]
             (sf/query! client (str "select Amount__c from Payment__c "
                                    "where Amount__c = 5"))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}}]
             (sf/query! client (str "select Amount__c from Payment__c "
                                    "where Amount__c >= 5"))))
      (is (= []
             (sf/query! client (str "select Amount__c from Payment__c "
                                    "where Amount__c >= 6"))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}}]
             (sf/query! client (str "select Amount__c from Payment__c "
                                    "where not (Amount__c = 6)"))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}}]
             (sf/query! client (str "select Amount__c from Payment__c "
                                    "where (Amount__c = 5)"))))
      (is (not (seq (sf/query! client (str "select Amount__c from Payment__c "
                                           "where Amount__c = 0")))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}
               :CreatedBy {:Name "Donald" :attributes {:type "User"}}}]
             (sf/query! client (str "select Amount__c,CreatedBy.Name from Payment__c"))))
      (is (= 1 (sf/count! client "select Amount__c from Payment__c")))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}
               :CreatedBy {:Name "Donald" :attributes {:type "User"}}}]
             (sf/query! client (str "select Amount__c, CreatedBy.Name from Payment__c "
                                    "where CreatedBy.Name = 'Donald'"))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}
               :CreatedBy {:Name "Donald" :attributes {:type "User"}}}]
             (sf/query! client (str "select Amount__c, CreatedBy.Name from Payment__c "
                                    "where CreatedBy.Name IN ('Donald', 'Claire')"))))
      (is (= []
             (sf/query! client (str "select Amount__c, CreatedBy.Name from Payment__c "
                                    "where CreatedBy.Name NOT IN ('Donald', 'Claire')")))))
    (testing "dates"
      (sf/create! client "Payment__c" {"Actual_Date__c" "2016-01-01"})
      (is (= (sq/query client {:find [:payment :actual-date]})
             [{:payment {}}
              {:payment {:actual-date (org.joda.time.LocalDate. "2016-01-01")}}]))
      (is (= [{:Actual_Date__c "2016-01-01"
               :attributes {:type "Payment__c"}}]
             (sf/query! client (str "select Amount__c, Actual_Date__c from Payment__c "
                                    "where Actual_Date__c = 2016-01-01"))))
      (is (not (seq
                (sf/query! client (str "select Amount__c, Actual_Date__c from Payment__c "
                                       "where Actual_Date__c = 2016-01-02"))))))
    (testing "limits"
      (is (= {} (sf/limits! client))))
    (testing "take-action!"
      (is (= "some input" (sf/take-action! client "custom/apex/TestEndpoint" "some input")))
      (is (thrown? clojure.lang.ExceptionInfo (sf/take-action! client "custom/apex/BadEndpoint" "other input"))))))
