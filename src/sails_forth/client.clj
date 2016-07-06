(ns sails-forth.client
  (:require [sails-forth.http :as http]
            [sails-forth.memory :as memory]))

(defprotocol Cache
  (put! [_ key value])
  (get! [_ key]))

(defprotocol Client
  (create!
   [_ type attrs]
   "Creates an object of the given type and attrs using the given salesforce
    client. If salesforce responds successfully, this returns the object's id,
    otherwise this raises an exception.")
  (delete!
   [_ type id]
   "Deletes the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise.")
  (update!
   [_ type id attrs]
   "Updates the object of the given type with the given id. This returns true
    if it succeeds and raises an exception otherwise.")
  (list!
   [_ type]
   "Lists all objets of the given type")
  (describe!
   [_ type]
   "Describes the given type")
  (objects!
   [_]
   "Lists all objects")
  (query!
   [_ query]
   "Executes the given query and returns all results, eagerly fetching if there
    is pagination")
  (count!
   [_ query]
   "Returns the number of results from the given query")
  (limits!
   [_]
   "Returns the current limits")
  (cache
   [_]
   "Returns a persistent cache"))

(defn build-atomic-cache
  []
  (let [state (atom {})]
    (reify Cache
      (put! [_ key value]
        (swap! state assoc-in [::cache key] value))
      (get! [_ key]
        (get-in @state [::cache key])))))

(defn build-http-client
  [config]
  (let [client (http/build-client! config)
        cache (build-atomic-cache)]
    (reify Client
      (create! [_ type attrs]
        (http/create! client type attrs))
      (delete! [_ type id]
        (http/delete! client type id))
      (update! [_ type id attrs]
        (http/update! client type id attrs))
      (list! [_ type]
        (http/list! client type))
      (describe! [_ type]
        (http/describe! client type))
      (objects! [_]
        (http/objects! client))
      (query! [_ query]
        (http/query! client query))
      (count! [_ query]
        (http/count! client query))
      (limits! [_]
        (http/limits! client))
      (cache [_]
        cache))))

(defn build-memory-client
  [schema]
  (let [client (memory/create-state! schema)
        cache (build-atomic-cache)]
    (reify Client
      (create! [_ type attrs]
        (memory/create! client type attrs))
      (delete! [_ type id]
        (memory/delete! client type id))
      (update! [_ type id attrs]
        (memory/update! client type id attrs))
      (list! [_ type]
        (memory/list! client type))
      (describe! [_ type]
        (memory/describe! client type))
      (objects! [_]
        (memory/objects! client))
      (query! [_ query]
        (memory/query! client query))
      (count! [_ query]
        (memory/count! client query))
      (limits! [_]
        (memory/limits! client))
      (cache [_]
        cache))))
