(ns sails-forth.datomic
  "Provides fns to assert the results of salesforce queries as datoms."
  (:require [clj-time.coerce :as tc]
            [clojure.set :as set]
            [clojure.string :as s]
            [sails-forth.client :as c]
            [sails-forth.clojurify :as clj]
            [sails-forth.query :as q]))

(def datomic-types
  {"datetime" :db.type/instant
   "date" :db.type/instant
   "int" :db.type/long
   "percent" :db.type/bigdec
   "currency" :db.type/bigdec
   "id" :db.type/string
   "string" :db.type/string
   "reference" :db.type/ref
   "boolean" :db.type/boolean
   "textarea" :db.type/string
   "picklist" :db.type/string
   "url" :db.type/uri
   "multipicklist" :db.type/string
   "phone" :db.type/string
   "address" :db.type/ref
   "email" :db.type/string
   "encryptedstring" :db.type/string})

(defn field-ident
  [ns-prefix field-name]
  (keyword (str (name ns-prefix) ".field") (name field-name)))

(defn field-attr
  [ns-prefix object-name field-name]
  (keyword (str (name ns-prefix) ".object." (name object-name)) (name field-name)))

(defn compound-ident
  [ns-prefix compound-name field-name]
  (keyword (str (name ns-prefix) ".compoound." (name compound-name)) (name field-name)))

(defn metadata-schema
  [ns-prefix]
  ;; More field metadata could come along for the ride
  [{:db/ident (field-ident ns-prefix "name")
    :db/doc "Salesforce field name"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (field-ident ns-prefix "type")
    :db/doc "Salesforce field type"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (field-ident ns-prefix "formula")
    :db/doc "Salesforce field formula"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (field-ident ns-prefix "helptext")
    :db/doc "Salesforce field help text"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   {:db/ident (compound-ident ns-prefix "address" "street")
    :db/doc "Salesforce address street"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (compound-ident ns-prefix "address" "city")
    :db/doc "Salesforce address city"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (compound-ident ns-prefix "address" "state-code")
    :db/doc "Salesforce address state"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (compound-ident ns-prefix "address" "postal-code")
    :db/doc "Salesforce address postal code"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident (compound-ident ns-prefix "address" "country-code")
    :db/doc "Salesforce address country"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(defn object-schema
  [ns-prefix object-key fields]
  (letfn [(field-datoms [[key field]]
            (let [{:keys [name
                          label
                          type
                          calculatedFormula
                          inlineHelpText
                          unique]}
                  field
                  cardinality (if (= "multipicklist" type)
                                :db.cardinality/many
                                :db.cardinality/one)
                  recordtype? (= key :recordtype)
                  valuetype (cond
                              recordtype? :db.type/string
                              (= "double" type)
                              (case (clj/double-type field)
                                :long :db.type/long
                                :bigint :db.type/bigint
                                :bigdec :db.type/bigdec)
                              :else
                              (get datomic-types type))]
              (cond-> {:db/ident (field-attr ns-prefix object-key key)
                       :db/doc label
                       :db/valueType valuetype
                       :db/cardinality cardinality
                       (field-ident ns-prefix "name") name
                       (field-ident ns-prefix "type") type}
                ;; Sorta funny that id types don't have :unique true
                (or (= "id" type) unique)
                (assoc :db/unique :db.unique/identity)
                (= "address" type)
                (assoc :db/isComponent true)
                calculatedFormula
                (assoc (field-ident ns-prefix "formula") calculatedFormula)
                inlineHelpText
                (assoc (field-ident ns-prefix "helptext") inlineHelpText))))]
    (into [] (map field-datoms) fields)))

(defn build-schema
  [client ns-prefix object-keys]
  [(metadata-schema ns-prefix)
   (into []
         (mapcat (fn [object-key]
                   (object-schema ns-prefix object-key (c/get-fields client object-key))))
         object-keys)])

(defn assert-object
  [client ns-prefix object-key m]
  (let [fields (c/get-fields client object-key)]
    (reduce-kv (fn [txn field-key value]
                 (let [attr  (field-attr ns-prefix object-key field-key)
                       field (get fields field-key)
                       {:keys [type referenceTo]} field
                       recordtype? (= field-key :recordtype)
                       [value ref-types]
                       (case type
                         "multipicklist"
                         [(s/split value #";")]
                         "date"
                         [(tc/to-date value)]
                         "reference"
                         (if-not recordtype?
                           (let [ref-key (clj/field->refers-attr field)
                                 ref-object (assert-object client ns-prefix ref-key value)]
                             [(dissoc ref-object ::types)
                              (get ref-object ::types)])
                           [(get value :name)])
                         "address"
                         (let [{:keys [street city stateCode postalCode countryCode]} value
                               attr (partial compound-ident ns-prefix "address")]
                           [(cond-> {}
                              street (assoc (attr "street") street)
                              city (assoc (attr "city") city)
                              stateCode (assoc (attr "state-code") stateCode)
                              postalCode (assoc (attr "postal-code") postalCode)
                              countryCode (assoc (attr "country-code") countryCode))])
                         [value])]
                   (-> txn
                       (assoc attr value)
                       (cond-> (seq ref-types)
                         (update ::types into ref-types)))))
               {::types #{object-key}}
               m)))

(defn assert-query
  "Returns a seq of transaction seqs that if transacted in order will assert
   the results of the given query in a datomic database.

   Given an ns-prefix of `\"sf\"` and a query of
   `{:find [:customer :id :sectors [:contact :id :phone]]}`:

   The first transaction asserts a set of attributes that will be defined on the
   attributes that will model the salesforce fields where there is no direct
   datomic analog, e.g. `sf.field/helptext`.

   The second transaction asserts a set of attributes that model the
   fields of objects used in the query,
   e.g. `sf.object.customer/id`. Note this is a complete set of
   attributes, not limited simply to those used in the query.

   The last transaction asserts the entities returned by the query.

   Most field values have natural datomic types. Notable exceptions include:

   * picklist, multipicklist: stored as strings. The api does not provide any
     access to inactive picklist items, which makes asserting e.g. enum values
     problematic
   * recordtype references: stored as strings for similar reasons
   * address: stored as component references

   There are some modest restrictions on the queries that can be asserted.
   All join references must include an identity field, except for recordtype
   joins which must only include the `:name` field."
  [client ns-prefix query]
  (let [objects (into []
                      (comp (map (comp first seq))
                            (map (partial apply assert-object client ns-prefix)))
                      (q/query client query))
        object-keys (reduce set/union #{} (map ::types objects))]
    (conj (build-schema client ns-prefix object-keys)
          (into [] (map (fn [m] (dissoc m ::types)) objects)))))
