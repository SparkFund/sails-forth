(ns sails-forth.memory-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer :all]
            [sails-forth.client :as sf]
            [sails-forth.memory :refer :all]
            [sails-forth.query :as sq])
  (:refer-clojure :exclude [update list]))

(deftest test-memory-client
  (let [schema (edn/read-string (slurp "test/schema.edn"))
        client (sf/build-memory-client schema)]
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
      (is (not (seq (sf/query! client (str "select Amount__c from Payment__c "
                                           "where Amount__c = 0")))))
      (is (= [{:Amount__c 5 :attributes {:type "Payment__c"}
               :CreatedBy {:Name "Donald" :attributes {:type "User"}}}]
             (sf/query! client (str "select Amount__c,CreatedBy.Name from Payment__c")))))
    #_(do
      (sf/create! client "opportunity" {:name "foo"})
      (sf/create! client "opportunity" {:name "bar"})
      (sf/create! client "opportunity" {:name "baz"})
      (sf/delete! client "opportunity" 2)
      (sf/update! client "opportunity" 3 {:name "qat"})
      (is (= [{:id 1 :name "foo"} {:id 3 :name "qat"}]
             (sf/list! client "opportunity")))
      (is (= [{:id 3 :name "qat"}]
             (sf/query! client (str "select id, name from opportunity "
                                    "where name = 'qat'"))))
      (sf/create! client "payment" {:name "foo 1" :opportunity 1})
      (is (= [{:opportunity {:name "foo"}}]
             (sf/query! client "select opportunity.name from payment")))
      (is (= [{:payment {:name "foo 1" :opportunity {:name "foo"}}}]
             (sq/query client {:find [:payment :name [:opportunity :name]]}))))))
