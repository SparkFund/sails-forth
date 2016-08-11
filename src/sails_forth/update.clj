(ns sails-forth.update
  "Execute SalesForce updates using more idiomatic Clojure syntax."
  (:require [clojure.spec :as s]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :as sc :refer :all]
            [sails-forth.spec :as spec]))

(s/fdef update!
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :object-id ::spec/id
               :values (s/map-of ::sc/attr ::sc/value))
  :ret #{true})

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
                                [sf-k (render-value desc v)]))
                          (into {}))]
    (when-not sf-type
      (throw (ex-info (str "no SalesForce type for " type)
                      {:description (get-type-description client type)})))
    (sf/update! client
                sf-type
                object-id
                sf-value-map)))

(s/fdef create!
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :values (s/map-of ::sc/attr ::sc/value))
  :ret ::spec/id)

(defn create!
  [client type new-value-map]
  (let [sf-type (:name (get-type-description client type))
        sf-value-map (->> (for [[k v] new-value-map
                                :let [desc (get-field-description client type k)
                                      sf-k (:name desc)]]
                            (do (when-not desc
                                  (throw (ex-info (str "no SalesForce attribute for " k)
                                                  {:description desc})))
                                [sf-k (render-value desc v)]))
                          (into {}))]
    (when-not sf-type
      (throw (ex-info (str "no SalesForce type for " type)
                      {:description (get-type-description client type)})))
    (sf/create! client
                sf-type
                sf-value-map)))
