(ns sails-forth.update
  "Execute SalesForce updates using more idiomatic Clojure syntax."
  (:require [clojure.core.typed :as t]
            [sails-forth :as sf]
            [sails-forth.clojurify :refer :all]))

(t/ann update! [sf/SalesforceClient Keyword sf/SalesforceId (t/Map t/Keyword t/Any)])
(defn update!
  "Performs updates on the object `object-id` with the given `type`."
  [client type object-id new-value-map]
  (let [sf-type (:name (get-type-description client type))
        sf-value-map (->> (for [[k v] new-value-map
                                :let [desc (get-field-description client type k)
                                      sf-k (:name desc)]]
                            (do (when-not desc
                                  (throw (ex-info (str "no SalesForce attribute for " k)
                                                  {:description desc})))
                                [sf-k v]))
                          (into {}))]
    (when-not sf-type
      (throw (ex-info (str "no SalesForce type for " type)
                      {:description (get-type-description client type)})))
    (sf/update! client
                sf-type
                object-id
                sf-value-map)))

(t/ann create! [sf/SalesforceClient Keyword (t/Map t/Keyword t/Any)])
(defn create!
  [client type new-value-map]
  (let [sf-type (:name (get-type-description client type))
        sf-value-map (->> (for [[k v] new-value-map
                                :let [desc (get-field-description client type k)
                                      sf-k (:name desc)]]
                            (do (when-not desc
                                  (throw (ex-info (str "no SalesForce attribute for " k)
                                                  {:description desc})))
                                [sf-k v]))
                          (into {}))]
    (when-not sf-type
      (throw (ex-info (str "no SalesForce type for " type)
                      {:description (get-type-description client type)})))
    (sf/create! client
                sf-type
                sf-value-map)))
