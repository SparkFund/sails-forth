(ns sails-forth.query
  "Provides for executing queries using more idiomatic clojure forms"
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.core.typed :as t]
            [clojure.string :as string]
            [sails-forth :as sf]))

;; TODO clarify types and add types for where clauses

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
  (if-let [types (get-in @client [::types])]
    types
    (let [objects (sf/objects! client)
          {:keys [sobjects]} objects
          type->object (->> sobjects
                            (map (juxt object->attr identity))
                            set-map)]
      (swap! client (fn [state] (assoc state ::types type->object)))
      type->object)))

(t/ann ^:no-check get-type-description
       [sf/SalesforceClient Attr -> (t/Option sf/SalesforceObjectDescription)])
(defn get-type-description
  "Obtains the description for a given type and builds some custom indexes
   into it. This will only fetch the type once for a given client."
  [client type]
  (when-let [overview (type (get-types client))]
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
                                 ::label-index label-index)]
          (swap! client (fn [state]
                          (assoc-in state [::types type] description)))
          description)))))

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

(t/defprotocol SoqlValue
  (soql-value [_] :- t/Str))

;; TODO figure out how to annotate these impls
(extend-protocol SoqlValue
  String
  (soql-value [s]
    (str "'" (string/replace s #"'" "\\'") "'"))
  clojure.lang.IPersistentSet
  (soql-value [xs]
    (str "(" (string/join "," (map soql-value xs)) ")"))
  clojure.lang.IPersistentVector
  (soql-value [fields]
    (loop [refs []
           fields fields]
      (if-not (seq fields)
        (string/join "." refs)
        (let [field (first fields)
              fields' (next fields)]
          (when (and (not fields')
                     (= "reference" (:type field)))
            (throw (ex-info "Invalid field path"
                            {:field-path fields})))
          (let [ref (if (= "reference" (:type field))
                      (:relationshipName field)
                      (:name field))]
            (recur (conj refs ref)
                   fields')))))))

(t/defalias WhereOp
  "Operators allowed in where clauses"
  (t/U ':in ':= ':or))

(t/defalias WhereSimpleValue
  (t/U t/Str))

(t/defalias WhereValue
  (t/U WhereSimpleValue (t/Set WhereSimpleValue) FieldPath))

(t/defalias WhereClause
  '[WhereOp WhereValue WhereValue])

(declare soql-where*)

(t/ann ^:no-check soql-where
       [WhereClause -> t/Str])
(defn soql-where
  [[op & args]]
  ;; TODO the type of op is significant
  (case op
    :or (str "(" (string/join ") OR (" (map soql-where* args)) ")")
    (let [[lh rh] args]
      (str "(" (soql-value lh) " " (name op) " " (soql-value rh) ")"))))

(t/ann ^:no-check soql-where*
       [(t/Seqable WhereClause) -> t/Str])
(defn soql-where*
  [where*]
  (string/join " AND " (map soql-where where*)))

(t/ann ^:no-check soql-query
       [sf/SalesforceClient t/Keyword (t/NonEmptyVec FieldPath) (t/Vec WhereClause) -> t/Str])
(defn soql-query
  "Creates a soql query string for the given client, type, and seq of field
   paths"
  [client type field-paths where]
  (let [description (get-type-description client type)
        soql-fields (map soql-value field-paths)]
    (str "SELECT " (string/join "," soql-fields)
         " FROM " (:name description)
         (when (seq where)
           (str " WHERE " (soql-where* where))))))

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

(defn update-attr-path [client where]
  (mapv (fn [[op & args :as clause]]
          (case op
            :or `[:or ~@(map (partial update-attr-path client) args)]
            (update clause 1 (fn [path] (resolve-attr-path client (first path) (rest path))))))
        where))

(t/ann ^:no-check query-attr-paths
       [sf/SalesforceClient t/Keyword (t/NonEmptyVec AttrPath) (t/Vec WhereClause) -> (t/Vec t/Any)])
(defn query-attr-paths
  "Queries the given client and type for the given seq of attr-paths, e.g.
   [[:account :name] [:account :createdby :lastname]]. This returns a vector
   of maps with keyword paths matching each of the attr-paths, e.g.
   {:account {:name ... :createdby {:lastname ...}}}. The base type will also
   have metadata with a :url resolvable by the current client."
  [client type attr-paths where]
  (let [field-paths (mapv (partial resolve-attr-path client type) attr-paths)
        where (update-attr-path client where)
        soql (soql-query client type field-paths where)
        records (sf/query! client soql)]
    (mapv (fn [record]
            (let [url (get-in record [:attributes :url])]
              (with-meta
                (reduce (fn [record' [field-path attr-path]]
                          (let [record-path (resolve-field-path field-path)
                                value (get-in record record-path)
                                field (last field-path)]
                            (cond-> record'
                              value
                              (assoc-in attr-path (parse-value field value)))))
                        {}
                        (map vector field-paths attr-paths))
                {:url url})))
          records)))

(t/defalias Variant
  (t/Rec [v]
         (t/HVec [t/Keyword (t/U t/Keyword v) *])))

(t/ann ^:no-check expand-variants
       [Variant -> (t/Vec AttrPath)])
(defn expand-variants
  "Expands a variant path into a seq of attr paths"
  [variant-path]
  (let [[type & refs] variant-path]
    (reduce (fn [accum ref]
              (if (sequential? ref)
                (into accum (mapv (partial into [type]) (expand-variants ref)))
                (conj accum [type ref])))
            []
            refs)))

(t/defalias Query
  (t/HMap :mandatory {:find Variant}
          :optional {:where (t/Vec WhereClause)}
          :complete? true))

(t/ann ^:no-check query
       [sf/SalesforceClient Query -> (t/Vec t/Any)])
(defn query
  "Returns the results of the given query against the given client. The query is
   a map with a :find keyword whose value must be a vector of keywords and
   vectors; the first position of any vector is taken to be a reference type.
   For example, [:account :name [:createdby :lastname]]. The result will be a
   vector of maps whose structures are given by the find clause, e.g.
   {:account {:name ... :createdby {:lastname ...}}}"
  [client query]
  (let [attr-paths (expand-variants (:find query))]
    (when (seq attr-paths)
      (let [type (ffirst attr-paths)]
        (mapv (fn [record]
                {type record})
              (query-attr-paths client type (map next attr-paths) (:where query)))))))
