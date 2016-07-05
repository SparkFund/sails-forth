(ns sails-forth.memory-test
  (:require [clojure.test :refer :all]
            [sails-forth.client :as sf]
            [sails-forth.memory :refer :all]
            [sails-forth.query :as sq])
  (:refer-clojure :exclude [update list]))

(deftest test-memory-client
  (let [schema {:opportunity {:name "string"}
                :payment {:name "string"
                          :opportunity :opportunity}}
        client (sf/build-memory-client schema)]
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
           (sq/query client {:find [:payment :name [:opportunity :name]]})))))
