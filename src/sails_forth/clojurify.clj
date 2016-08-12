(ns sails-forth.clojurify
  "Translates between SalesForce string API type/attribute names and
  more Clojure-y keywords."
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.spec :as s]
            [clojure.string :as string]
            [sails-forth.client :as sf]
            [sails-forth.memory :as memory]
            [sails-forth.spec :as spec])
  (:import [org.joda.time DateTime LocalDate]))

(s/def ::attr
  keyword?)

(s/def ::attr-path
  (s/coll-of ::attr :kind vector? :min-count 1))

(s/def ::field-path
  (s/coll-of ::spec/field-description :kind vector? :min-count 1))

(s/fdef field->attr
  :args (s/cat :field ::spec/field-description)
  :ret ::attr)

(defn field->attr
  "Derives a clojurey attribute keyword representation of a Salesforce field.
   This converts snake case to kebob case, removes any custom field suffix,
   and removes the Id suffix from native reference types."
  [field]
  (let [{:keys [name type]} field
        name' (string/replace name #"__c\Z" "")
        custom? (not= name name')]
    (-> name'
        (cond->
          (and (= type "reference")
               (not custom?))
          (string/replace #"Id\Z" ""))
        string/lower-case
        (string/replace \_ \-)
        keyword)))

(s/fdef field->refers-attr
  :args (s/cat :field ::spec/field-description)
  :ret ::attr)

(defn field->refers-attr
  "Derives a clojurey attribute keyword representation of the Salesforce
   relation about which this field refers"
  [field]
  (let [{:keys [referenceTo type]} field]
    (when (or (not= "reference" type)
              (not (and (= 1 (count referenceTo))
                        (string? (first referenceTo)))))
      (throw (ex-info "Invalid reference field" {:field field})))
    (-> referenceTo
        first
        (string/replace #"__c\Z" "")
        string/lower-case
        (string/replace \_ \-)
        keyword)))

(s/fdef object->attr
  :args (s/cat :object ::spec/object-overview)
  :ret ::attr)

(defn object->attr
  "Derives a clojurey type keyword representation of a Salesforce object.
   This converts snake case to kebob case and removes any custom field suffix."
  [object]
  (let [{:keys [name custom]} object
        name' (string/replace name #"__c\Z" "")]
    (-> name'
        string/lower-case
        (string/replace #"_{1,2}" "-")
        keyword)))

(s/fdef set-map
  :args (s/and (s/coll-of (s/tuple keyword? any?))
               #(= (count %) (count (set (map first %)))))
  :ret (s/map-of keyword? any?))

(defn set-map
  "Builds a map from the given seq of entries, raising on any duplicate key"
  [entries]
  (reduce (fn [accum [k v]]
            (when (contains? accum k)
              (let [msg "Duplicate key given for map"]
                (throw (ex-info msg {:key k :entries entries}))))
            (assoc accum k v))
          {}
          entries))

(s/fdef get-types
  :args (s/cat :client ::sf/client)
  :ret (s/map-of ::attr ::spec/object-overview))

(defn get-types
  "Obtains a map of descriptions by type"
  [client]
  (if-let [types (sf/get! (sf/cache client) ::types)]
    types
    (let [objects (sf/objects! client)
          {:keys [sobjects]} objects
          type->object (->> sobjects
                            (map (juxt object->attr identity))
                            set-map)]
      (sf/put! (sf/cache client) ::types type->object)
      type->object)))

(s/fdef get-type-description
  :args (s/cat :client ::sf/client
               :type ::attr)
  :ret (s/nilable ::spec/object-description))

(defn get-type-description
  "Obtains the description for a given type and builds some custom indexes
   into it. This will only fetch the type once for a given client."
  [client type]
  (let [types (get-types client)]
    (when-let [overview (type types)]
      (if (:fields overview)
        overview
        (when-let [description (sf/describe! client (:name overview))]
          (let [{:keys [fields]} description
                attr->field (->> fields
                                 (map (juxt field->attr identity))
                                 set-map)
                field-index (->> fields
                                 (map (juxt (comp keyword :name) identity))
                                 set-map)
                label-index (reduce (fn [accum field]
                                      (let [attr (field->attr field)
                                            {:keys [label]} field]
                                        (update accum label (fnil conj #{}) attr)))
                                    {}
                                    fields)
                description (assoc description
                                   ::attr->field attr->field
                                   ::field-index field-index
                                   ::label-index label-index)
                updated (merge overview description)]
            (sf/put! (sf/cache client) ::types (assoc types type updated))
            updated))))))

(s/fdef get-fields
  :args (s/cat :client ::sf/client
               :type ::attr)
  :ret (s/nilable (s/map-of ::attr ::spec/field-description)))

(defn get-fields
  "Obtains a map of descriptions by field for the given type"
  [client type]
  (::attr->field (get-type-description client type)))

(s/fdef get-field-description
  :args (s/cat :client ::sf/client
               :type ::attr
               :attr ::attr)
  :ret (s/nilable ::spec/field-description))

(defn get-field-description
  "Obtains the description for the given field on a type by its attribute"
  [client type attr]
  (let [type-description (get-type-description client type)]
    (get-in type-description [::attr->field attr])))

(s/fdef get-attrs-for-label
  :args (s/cat :client ::sf/client
               :type ::attr
               :label string?)
  :ret (s/coll-of ::attr :kind set?))

(defn get-attrs-for-label
  "Returns the set of attributes on the given type that have the given label"
  [client type label]
  (let [description (get-type-description client type)]
    (get (::label-index description) label)))

(s/def ::datetime
  (partial instance? DateTime))

(s/def ::date
  (partial instance? LocalDate))

(s/def ::value
  (s/or :datetime ::datetime
        :date ::date
        :int integer?
        :bigdec bigdec?
        :other ::spec/json-simple))

(s/fdef parse-value
  :args (s/cat :field ::spec/field-description
               :value ::spec/json-simple)
  :ret ::value
  ;; TODO could specify that render-value is the inverse
  )

(def parse-value
  "Parses the given value according to its field type and other characteristics"
  (let [date-time-formatter (tf/formatters :date-time)
        date-formatter (tf/formatters :date)
        max-long-precision (dec (count (str Long/MAX_VALUE)))]
    (fn [field value]
      (let [{:keys [type scale precision]} field]
        (case type
          "datetime" (tf/parse date-time-formatter value)
          "date" (tc/to-local-date (tf/parse date-formatter value))
          "double" (if (= 0 scale)
                     (if (<= precision max-long-precision)
                       (long value)
                       (bigint value))
                     value)
          "int" (long value)
          "percent" (/ value 100M)
          value)))))

(s/fdef render-value
  :args (s/cat :field ::spec/field-description
               :value ::value)
  :ret ::spec/json-simple
  ;; TODO could specify that parse-value is the inverse
  )

(def render-value
  "Parses the given value according to its field type and other characteristics"
  (let [date-time-formatter (tf/formatters :date-time)
        date-formatter (tf/formatters :date)]
    (fn [field value]
      (let [{:keys [type scale precision]} field]
        (case type
          "datetime" (tf/unparse date-time-formatter (tc/to-date-time value))
          "date" (tf/unparse date-formatter (tc/to-date-time value))
          "percent" (* value 100M)
          value)))))

(s/fdef resolve-attr-path
  :args (s/cat :client ::sf/client
               :type ::attr
               :attr-path ::attr-path)
  :ret ::field-path)

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
                 (field->refers-attr field))
               attr-path'
               (conj fields field))))))

(s/fdef resolve-field-path
  :args (s/cat :field-path ::field-path)
  :ret ::attr-path)

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
  :args (s/cat :client ::sf/client
               :types (s/coll-of ::attr))
  :ret ::memory/schema)

(defn schema
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
                  type-schema (-> (sf/describe! client type-name)
                                  (select-keys type-attrs)
                                  (update :fields (partial map #(select-keys % field-attrs))))]
              [type-name type-schema])))))
