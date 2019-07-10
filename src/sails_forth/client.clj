(ns sails-forth.client
  (:require [clojure.spec.alpha :as s]
            [sails-forth.http :as http]
            [sails-forth.memory2 :as memory2]
            [sails-forth.spec :as spec]
            [sails-forth.clojurify :as clj]))

(defprotocol Cache
  (^:spark/no-boot-spec-coverage
    put! [_ key value])
  (^:spark/no-boot-spec-coverage
    get! [_ key]))

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
  (^:spark/no-boot-spec-coverage
    create!
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
  (^:spark/no-boot-spec-coverage
    list!
    [_ type]
    "Lists all objets of the given type")
  (^:spark/no-boot-spec-coverage
    describe!
    [_ type]
    "Describes the given type")
  (^:spark/no-boot-spec-coverage
    objects!
   [_]
   "Lists all objects")
  (^:spark/no-boot-spec-coverage
    query!
    [_ query]
    "Executes the given query and returns all results, eagerly fetching if there
     is pagination")
  (^:spark/no-boot-spec-coverage 
    count!
    [_ query]
    "Returns the number of results from the given query")
  (^:spark/no-boot-spec-coverage
    limits!
    [_]
    "Returns the current limits")
  (cache
    [_]
    "Returns a persistent cache")
  (^:spark/no-boot-spec-coverage
    import!
    [_ type records]
    "Imports the given records into the given type")
  (list-actions!
    [_ path]
    "Gets a list of actions that can be performed")
  (describe-action!
    [_ action]
    "Describes an action")
  (take-action!
    [_ action inputs]
    "Submits a request to perform the given action"))

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
        (= (count (:records args)) (count @ret))))

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

(defn ^:spark/no-boot-spec-coverage build-http-client
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
        cache)
      (list-actions! [_ subtype]
        (http/list-actions! client subtype))
      (describe-action! [_ action]
        (http/describe-action! client action))
      (take-action! [_ action inputs]
        (http/take-action! client action inputs)))))

(s/def ::take-action-map
  (s/? (s/map-of string? (s/fdef f :args (s/cat :client any?
                                                :inputs (s/coll-of (s/map-of string? any?)))
                                   :ret any?))))

(s/fdef build-memory-client
  :args (s/cat :schema ::memory2/schema
               :take-action-map ::take-action-map)
  :ret ::client)

(defn build-memory-client
  ([schema]
   (build-memory-client schema {}))
  ([schema take-action-map]
   (let [client (memory2/create-state! schema)
         cache (build-atomic-cache)]
     (reify Client
       (create! [_ type attrs]
         (memory2/create! client type attrs))
       (delete! [_ type id]
         (memory2/delete! client type id))
       (update! [_ type id attrs]
         (memory2/update! client type id attrs))
       (list! [_ type]
         (memory2/list! client type))
       (describe! [_ type]
         (memory2/describe! client type))
       (objects! [_]
         (memory2/objects! client))
       (query! [_ query]
         (memory2/query! client query))
       (count! [_ query]
         (memory2/count! client query))
       (limits! [_]
         (memory2/limits! client))
       (import! [_ type records]
         (future (mapv (partial create! type) records)))
       (cache [_]
         cache)
       (take-action! [_ action inputs]
         (memory2/take-action! client take-action-map action inputs))))))

(defn client?
  [x]
  (and (extends? Client (class x)) x))

(s/fdef get-types
  :args (s/cat :client ::client)
  :ret (s/map-of ::clj/attr ::spec/object-overview))

(defn get-types
  "Obtains a map of descriptions by type"
  [client]
  (if-let [types (get! (cache client) ::types)]
    types
    (let [objects (objects! client)
          {:keys [sobjects]} objects
          type->object (->> sobjects
                            (map (juxt clj/object->attr identity))
                            clj/set-map)]
      (put! (cache client) ::types type->object)
      type->object)))

(s/fdef get-type-description
  :args (s/cat :client ::client
               :type ::clj/attr)
  :ret (s/nilable ::spec/object-description))

(defn get-type-description
  "Obtains the description for a given type and builds some custom indexes
   into it. This will only fetch the type once for a given client."
  [client type]
  (let [types (get-types client)]
    (when-let [overview (type types)]
      (if (:fields overview)
        overview
        (when-let [description (describe! client (:name overview))]
          (let [{:keys [fields]} description
                attr->field (->> fields
                                 (map (juxt clj/field->attr identity))
                                 clj/set-map)
                field-index (->> fields
                                 (map (juxt (comp keyword :name) identity))
                                 clj/set-map)
                label-index (reduce (fn [accum field]
                                      (let [attr (clj/field->attr field)
                                            {:keys [label]} field]
                                        (update accum label (fnil conj #{}) attr)))
                                    {}
                                    fields)
                description (assoc description
                                   ::attr->field attr->field
                                   ::field-index field-index
                                   ::label-index label-index)
                updated (merge overview description)]
            (put! (cache client) ::types (assoc types type updated))
            updated))))))

(s/fdef get-fields
  :args (s/cat :client ::client
               :type ::clj/attr)
  :ret (s/nilable (s/map-of ::clj/attr ::spec/field-description)))

(defn ^:spark/no-boot-spec-coverage get-fields
  "Obtains a map of descriptions by field for the given type"
  [client type]
  (::attr->field (get-type-description client type)))

(s/fdef get-field-description
  :args (s/cat :client ::client
               :type ::clj/attr
               :attr ::clj/attr)
  :ret (s/nilable ::spec/field-description))

(defn get-field-description
  "Obtains the description for the given field on a type by its attribute"
  [client type attr]
  (let [type-description (get-type-description client type)]
    (get-in type-description [::attr->field attr])))

(s/fdef get-attrs-for-label
  :args (s/cat :client ::client
               :type ::clj/attr
               :label string?)
  :ret (s/coll-of ::clj/attr :kind set?))

(defn ^:spark/no-boot-spec-coverage get-attrs-for-label
  "Returns the set of attributes on the given type that have the given label"
  [client type label]
  (let [description (get-type-description client type)]
    (get (::label-index description) label)))

(s/fdef resolve-attr-path
  :args (s/cat :client ::client
               :type ::clj/attr
               :attr-path ::clj/attr-path)
  :ret ::clj/field-path)

(defn resolve-attr-path
  "Resolves a path of attrs against a given type, returning a path of fields.
   All but the last attr in a path must resolve to a reference type."
  [client type attr-path]
  (loop [type type
         attr-path attr-path
         fields []]
    (if-not (seq attr-path)
      fields
      (let [attr (first attr-path)
            field (get-field-description client type attr)
            attr-path' (next attr-path)]
        (when-not field
          (throw (ex-info "Invalid attr path" {:attr-path attr-path
                                               :type type})))
        (recur (when attr-path'
                 (clj/field->refers-attr field))
               attr-path'
               (conj fields field))))))

(s/fdef resolve-field-path
  :args (s/cat :field-path ::clj/field-path)
  :ret ::clj/attr-path)

(defn resolve-field-path
  "Derives a seq of record keys for the given seq of fields, suitable for
   applying to the result of the underlying query! fn"
  [field-path]
  (loop [record-path []
         field-path field-path]
    (if-not (seq field-path)
      record-path
      (let [field (first field-path)
            field-path' (next field-path)]
        (when (and (not field-path')
                   (= "reference" (:type field)))
          (throw (ex-info "Invalid field path"
                          {:field-path field-path})))
        (let [record-key (keyword (if (= "reference" (:type field))
                                    (:relationshipName field)
                                    (:name field)))]
          (recur (conj record-path record-key)
                 field-path'))))))

(s/fdef schema
  :args (s/cat :client ::client
               :types (s/coll-of ::clj/attr))
  :ret ::memory2/schema)

(defn ^:spark/no-boot-spec-coverage schema
  [client types]
  (let [type-attrs #{:name :label :custom :fields}
        field-attrs #{:name
                      :type
                      :referenceTo
                      :scale
                      :precision
                      :label
                      :relationshipName
                      :picklistValues
                      :nillable
                      :defaultValue}
        all-types (get-types client)]
    (into {}
          (for [type types]
            (let [type-name ((comp :name all-types) type)
                  type-schema (-> (describe! client type-name)
                                  (select-keys type-attrs)
                                  (update :fields (partial map #(select-keys % field-attrs))))]
              [type-name type-schema])))))
