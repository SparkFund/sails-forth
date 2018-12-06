(ns sails-forth.graphql
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [com.walmartlabs.lacinia.executor :as executor]
            [com.walmartlabs.lacinia.schema :as schema]
            [sails-forth.client :as client]))

;; TODO these become simple fns in 0.31
(def default-custom-scalars
  {:Date {:parse (schema/as-conformer (fn parse-date [s]
                                        (java.time.LocalDate/parse s)))
          :serialize (schema/as-conformer str)}
   :DateTime {:parse (schema/as-conformer (fn parse-datetime [s]
                                            (java.time.ZonedDateTime/parse s)))
              :serialize (schema/as-conformer str)}
   :Money {:parse (schema/as-conformer (fn parse-money [s]
                                         (.setScale ^BigDecimal (bigdec s) 2)))
           :serialize (schema/as-conformer str)}
   :Time {:parse (schema/as-conformer (fn parse-time [s]
                                        (java.time.LocalTime/parse s)))
          :serialize (schema/as-conformer str)}})

(defn convert-name
  [s]
  (keyword s))

(defn picklist-enum-type
  [object field]
  (convert-name (str (get object :name) "_" (get field :name))))

(defn build-picklist-enum
  [field]
  (let [{:keys [name picklistValues]} field]
    {:values (into []
                   (map (fn [entry]
                          (let [{:keys [active label value]} entry]
                            (cond-> {:enum-value value}
                              label
                              (assoc :description label)
                              (not active)
                              (assoc :deprecated true)))))
                   picklistValues)}))

(defn resolve-soql-fields
  [sf-schema oname selection]
  (reduce-kv (fn [accum k v]
               (let [local-name (name k)]
                 (if (nil? v)
                   (conj accum local-name)
                   (let [selection (get v :selections)
                         object (first (filter (fn [field]
                                                 (= local-name (get field :name)))
                                               (get-in sf-schema [oname :fields])))
                         path (str (get object :relationshipName) ".")]
                     (into accum
                           (map (fn [field]
                                  (str path field)))
                           (resolve-soql-fields sf-schema local-name selection))))))
             #{}
             selection))

(defn build-query-by-id-resolver
  [sf-schema object field]
  (fn query-by-id-resolver [context args value]
    (let [{::keys [client]} context
          id (get args (convert-name (get field :name)))
          selection (executor/selections-tree context)
          fields (resolve-soql-fields sf-schema (get object :name) selection)
          soql (format "SELECT %s FROM %s WHERE %s = %s"
                       (string/join ", " fields)
                       (get object :name)
                       (get field :name)
                       ;; TODO SEVERE escape the id correctly
                       (str "'" id "'"))
          ;; TODO distinguish between id failures and other failures?
          results (client/query! client soql)]
      (when (seq results)
        (first results)))))

(defn add-field
  [schema sf-schema object field]
  (let [{:keys [queryable]} object
        {:keys [deprecatedAndHidden name label idLookup inlineHelpText relationshipName referenceTo type]} field
        description (or inlineHelpText label)
        enum-type? (case type
                     "combobox" true
                     "multipicklist" true
                     "picklist" true
                     false)
        ;; graphql enum values must be valid graphql identifiers, which is not
        ;; at all true of salesforce picklist values. Still, there's probably
        ;; value for schema exploration to make enums when we can, especially
        ;; since this schema is not intended to be a stable, reliable contract
        enum-values? (when enum-type?
                       (let [values (map :value (get field :picklistValues))]
                         (and (seq values)
                              (= (distinct values) values)
                              (every? (fn [value]
                                        (s/valid? ::schema/enum-value value))
                                      values))))
        enum-type (when (and enum-type? enum-values?)
                    (picklist-enum-type object field))
        gql-type (case type
                   "address" 'String ; TODO
                   "anyType" nil ; TODO wtf could we even do here? skip it for now.
                   "base64" 'String
                   "boolean" 'Boolean
                   "combobox" (or enum-type 'String)
                   "complexvalue" nil ; TODO wtf again
                   "currency" :Money ; TODO needs query resolver?
                   "date" :Date
                   "datetime" :DateTime
                   "double" 'Float
                   "email" 'String ; TODO
                   "encryptedstring" 'String
                   "id" 'ID
                   "int" 'Int
                   "multipicklist" (list 'list (or enum-type 'String))
                   "percent" 'Float
                   "phone" 'String ; TODO
                   "picklist" (or enum-type 'String)
                   "reference" (convert-name (first referenceTo))
                   "string" 'String
                   "textarea" 'String
                   "time" :Time
                   "url" 'String
                   (throw (ex-info "invalid type" {:field field})))]
    (cond-> schema
      gql-type
      (assoc-in [:objects (convert-name (get object :name))
                 :fields (convert-name name)]
                (cond-> {:type gql-type}
                  description
                  (assoc :description description)
                  deprecatedAndHidden
                  (assoc :deprecated true)
                  (= "reference" type)
                  (assoc :resolve (fn resolve-reference-field [context args value]
                                    (get value (keyword relationshipName))))))
      enum-type
      (assoc-in [:enums enum-type] (build-picklist-enum field))
      (and gql-type queryable idLookup)
      (assoc-in [:queries (convert-name (str (get object :name) "_by_" name))]
                {:type (convert-name (get object :name))
                 :args {(convert-name name) {:type (list 'non-null gql-type)}}
                 :resolve
                 (build-query-by-id-resolver sf-schema object field)}))))

(defn add-object
  [schema sf-schema object]
  (let [{:keys [name fields custom label]} object
        gql-object-path [:objects (convert-name name)]]
    (reduce (fn [schema field]
              (add-field schema sf-schema object field))
            (assoc-in schema (conj gql-object-path :description) label)
            fields)))

(defn build-schema
  [sf-schema]
  (reduce (fn [schema object] (add-object schema sf-schema object))
          {:scalars default-custom-scalars}
          (vals sf-schema)))
