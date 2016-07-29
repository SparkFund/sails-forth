(ns sails-forth.clojurify
  "Translates between SalesForce string API type/attribute names and
  more Clojure-y keywords."
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.core.typed :as t]
            [clojure.string :as string]
            [sails-forth.client :as sf]))

(t/defalias Attr
  "An Attr is a keyword that refers to a Salesforce object or field"
  t/Keyword)

(t/defalias AttrPath
  "An AttrPath is a vector of keywords that is resolved against a Salesforce
   object or database to yield an object or field"
  (t/NonEmptyVec Attr))

(t/defalias FieldPath
  (t/NonEmptyVec sf/SalesforceFieldDescription))

(t/ann ^:no-check field->attr
       [sf/SalesforceFieldDescription -> Attr])
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

(t/ann ^:no-check field->refers-attr
       [sf/SalesforceFieldDescription -> Attr])
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

(t/ann ^:no-check object->attr
       [sf/SalesforceObjectOverview -> Attr])
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

(t/ann ^:no-check set-map
       [(t/Seqable (t/HSeq [t/Keyword t/Any])) -> '{}])
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

(t/ann ^:no-check get-types
       [sf/SalesforceClient -> (t/Map Attr sf/SalesforceObjectOverview)])
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

(t/ann ^:no-check get-type-description
       [sf/SalesforceClient Attr -> (t/Option sf/SalesforceObjectDescription)])
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

(t/ann ^:no-check get-fields
       [sf/SalesforceClient Attr -> (t/Option (t/Map Attr sf/SalesforceFieldDescription))])
(defn get-fields
  "Obtains a map of descriptions by field for the given type"
  [client type]
  (::attr->field (get-type-description client type)))

(t/ann ^:no-check get-field-description
       [sf/SalesforceClient Attr Attr -> (t/Option sf/SalesforceFieldDescription)])
(defn get-field-description
  "Obtains the description for the given field on a type by its attribute"
  [client type attr]
  (let [type-description (get-type-description client type)]
    (get-in type-description [::attr->field attr])))

(t/ann ^:no-check get-attrs-for-label
       [sf/SalesforceClient Attr t/Str -> (t/Set Attr)])
(defn get-attrs-for-label
  "Returns the set of attributes on the given type that have the given label"
  [client type label]
  (let [description (get-type-description client type)]
    (get (::label-index description) label)))

(t/ann ^:no-check parse-value
       [sf/SalesforceFieldDescription t/Any -> t/Any])
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

(t/ann ^:no-check resolve-attr-path
       [sf/SalesforceClient Attr AttrPath -> FieldPath])
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

(t/ann ^:no-check resolve-field-path
       [FieldPath -> AttrPath])
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
