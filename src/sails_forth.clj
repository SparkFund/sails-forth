(ns sails-forth
  (:require [sails-forth.client :as client]))

(defn build-client!
  {:deprecated "0.3"}
  [config]
  (client/build-http-client config))

(defn create!
  {:deprecated "0.3"}
  [client type attrs]
  (client/create! client type attrs))

(defn delete!
  {:deprecated "0.3"}
  [client type attrs]
  (client/delete! client type attrs))

(defn update!
  {:deprecated "0.3"}
  [client type id attrs]
  (client/update! type type id attrs))

(defn list!
  {:deprecated "0.3"}
  [client type]
  (client/list! client type))

(defn describe!
  {:deprecated "0.3"}
  [client type]
  (client/describe! client type))

(defn objects!
  {:deprecated "0.3"}
  [client]
  (client/objects! client))

(defn query!
  {:deprecated "0.3"}
  [client query]
  (client/query! client query))

(defn count!
  {:deprecated "0.3"}
  [client query]
  (client/count! client query))

(defn limits!
  {:deprecated "0.3"}
  [client]
  (client/limits! client))
