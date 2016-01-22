(ns sails-forth.query
  "Provides for executing queries using more idiomatic clojure forms"
  (:require [clj-time.format :as tf]
            [clojure.string :as string]
            [sails-forth :as sf]))

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

(defn get-type-description
  "Obtains the description for a given type and builds some custom indexes
   into it. This will only fetch the type once for a given client."
  [client type]
  (if-let [description (get-in @client [::types type])]
    description
    (let [description (sf/describe! client (name type))
          {:keys [fields]} description
          attr->field (->> fields
                           (map (juxt field->attr identity))
                           (into {}))
          field-index (->> fields
                           (map (juxt (comp keyword :name) identity))
                           (into {}))
          description (assoc description
                             ::attr->field attr->field
                             ::field-index field-index)]
      (swap! client (fn [state]
                      (assoc-in state [::types type] description)))
      description)))

(defn get-field-description
  "Obtains the description for the given field on a type by its attribute"
  [client type attr]
  (let [type-description (get-type-description client type)]
    (get-in type-description [::attr->field attr])))

(def parse-value
  "Parses the given value according to its type"
  (let [date-formatter (tf/formatters :date-time)]
    (fn [type value]
      (case type
        "datetime" (tf/parse date-formatter value)
        value))))

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
          (throw (IllegalArgumentException. "Invalid attr path")))
        (when (and attr-path'
                   (or (not= "reference" (:type field))
                       (not (and (= 1 (count (:referenceTo field)))
                                 (string? (first (:referenceTo field)))))))
          (throw (IllegalArgumentException. "Invalid attr path")))
        (recur (some-> field :referenceTo first string/lower-case keyword)
               attr-path'
               (conj fields field))))))

(defn soql-field
  "Creates a soql field string for the given seq of fields"
  [field-path]
  (loop [ref-path []
         field-path field-path]
    (if-not (seq field-path)
      (string/join "." ref-path)
      (let [field (first field-path)
            field-path' (next field-path)]
        (when (and (not field-path')
                   (= "reference" (:type field)))
          (throw (IllegalArgumentException. "Invalid field path")))
        (let [ref (if (= "reference" (:type field))
                    (:relationshipName field)
                    (:name field))]
          (recur (conj ref-path ref)
                 field-path'))))))

;; TODO some support for WHERE clauses, obviously
(defn soql-query
  "Creates a soql query string for the given client, type, and seq of field
   paths"
  [client type field-paths]
  (let [soql-fields (map soql-field field-paths)]
    (str "SELECT " (string/join "," soql-fields)
         " FROM " (name type))))

(defn resolve-record-path
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
          (throw (IllegalArgumentException. "Invalid field path")))
        (let [record-key (keyword (if (= "reference" (:type field))
                                    (:relationshipName field)
                                    (:name field)))]
          (recur (conj record-path record-key)
                 field-path'))))))

(defn query-attr-paths
  "Queries the given client and type for the given seq of attr-paths, e.g.
   [[:account :name] [:account :createdby :lastname]]"
  [client type attr-paths]
  (let [field-paths (mapv (partial resolve-attr-path client type) attr-paths)
        soql (build-soql client type field-paths)
        records (sf/query! client soql)]
    (mapv (fn [record]
            (reduce (fn [record' [field-path attr-path]]
                      (let [record-path (resolve-record-path field-path)
                            value (get-in record record-path)
                            type (:type (last field-path))]
                        (cond-> record'
                          value
                          (assoc-in attr-path (parse-value type value)))))
                       {}
                       (map vector field-paths attr-paths)))
          records)))

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
              (query-attr-paths client type (map next attr-paths)))))))
