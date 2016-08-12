(ns sails-forth.client
  (:require [clojure.spec :as s]
            [sails-forth.http :as http]
            [sails-forth.memory :as memory]
            [sails-forth.spec :as spec]))

(defprotocol Cache
  (put! [_ key value])
  (get! [_ key]))

(s/def ::cache
  (partial satisfies? Cache))

(s/fdef put!
  :args (s/cat :cache ::cache
               :key any?
               :value any?))

(s/fdef get!
  :args (s/cat :cache ::cache
               :key any?)
  :ret any?)

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
    "Returns a persistent cache")
  (import!
    [_ type records]
    "Imports the given records into the given type"))

(s/def ::client
  (partial satisfies? Client))

(s/fdef create!
  :args (s/cat :client ::client
               :type ::spec/type
               :attrs ::spec/attrs)
  :ret ::spec/id)

(s/fdef delete!
  :args (s/cat :client ::client
               :type ::spec/type
               :id ::spec/id)
  :ret #{true})

(s/fdef update!
  :args (s/cat :client ::client
               :type ::spec/type
               :id ::spec/id
               :attrs ::spec/attrs)
  :ret #{true})

(s/fdef list!
  :args (s/cat :client ::client
               :type ::spec/type)
  :ret ::spec/json-map)

(s/fdef describe!
  :args (s/cat :client ::client
               :type ::spec/type)
  :ret ::spec/object-description)

(s/fdef objects!
  :args (s/cat :client ::client)
  :ret ::spec/objects-overview)

(s/fdef query!
  :args (s/cat :client ::client
               :query ::spec/query)
  :ret ::spec/records)

(s/fdef count!
  :args (s/cat :client ::client
               :query ::spec/query)
  :ret nat-int?)

(s/fdef limits!
  :args (s/cat :client ::client)
  :ret ::spec/limits)

(s/fdef import!
  :args (s/cat :client ::client
               :type ::spec/type
               :records (s/coll-of ::spec/attrs))
  :ret (s/and (partial instance? clojure.lang.IDeref)
              (comp (partial s/valid? (s/coll-of any?)) deref))
  :fn (fn [{:keys [args ret]}]
        (= (count (:data args)) (count @ret))))

(s/fdef build-atomic-cache
  :args (s/cat)
  :ret ::cache)

(defn build-atomic-cache
  []
  (let [state (atom {})]
    (reify Cache
      (put! [_ key value]
        (swap! state assoc-in [::cache key] value))
      (get! [_ key]
        (get-in @state [::cache key])))))

(s/fdef build-http-client
  :args (s/cat :config ::http/config)
  :ret ::client)

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
      (import! [_ type records]
        (http/import! client type records))
      (cache [_]
        cache))))

(s/fdef build-memory-client
  :args (s/cat :schema ::memory/schema)
  :ret ::client)

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
      (import! [_ type records]
        (future (mapv (partial create! type) records)))
      (cache [_]
        cache))))

(defn client?
  [x]
  (and (extends? Client (class x)) x))
