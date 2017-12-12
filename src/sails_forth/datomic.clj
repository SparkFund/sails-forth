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
   "double" :db.type/bigdec
   "currency" :db.type/bigdec
   "id" :db.type/string
   "string" :db.type/string
   "reference" :db.type/ref
   "boolean" :db.type/boolean
   "textarea" :db.type/string
   "picklist" :db.type/ref
   "url" :db.type/uri
   "multipicklist" :db.type/ref
   ;; maybe components? Do these have discrete pieces?
   ;; "address" :db.type/string
   ;; "phone" :db.type/string
   })

(defn field-ident
  [ns-prefix field-name]
  (keyword (str (name ns-prefix) ".field") (name field-name)))

(defn field-attr
  [ns-prefix object-name field-name]
  (keyword (str (name ns-prefix) ".object." (name object-name)) (name field-name)))

(defn enum-ident
  [ns-prefix object-name field-name value]
  (keyword (str (name ns-prefix) ".object." (name object-name) "." (name field-name)) (name value)))

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
   {:db/ident (field-ident ns-prefix "picklist-value")
    :db/doc "Salesforce field picklist value"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(defn picklist-name
  [picklist-value]
  (let [sb (StringBuffer.)]
    (doseq [c (seq picklist-value)]
      (cond (and (zero? (.length sb))
                 (Character/isDigit c))
            nil
            (or (Character/isLetterOrDigit c)
                (case c (\* \+ \! \- \_ \' \?) true false))
            (.append sb (Character/toLowerCase c))
            :else
            (.append sb \-)))
    (-> (.toString sb)
        (s/replace #"-+" "-")
        (s/replace #"^-|-$" ""))))

(defn object-schema
  [ns-prefix object-key fields]
  (letfn [(enum-datoms [key field]
            (letfn [(enum-datom [item]
                      (let [{:keys [label value]} item]
                        {:db/ident (enum-ident ns-prefix object-key key (picklist-name value))
                         :db/doc label
                         (field-ident ns-prefix "picklist-value") value}))]
              (map enum-datom (:picklistValues field))))
          (field-datoms [[key field]]
            (let [{:keys [name
                          label
                          type
                          calculatedFormula
                          inlineHelpText
                          unique]}
                  field
                  cardinality (if (= "multipicklist" type)
                                :db.cardinality/many
                                :db.cardinality/one)]
              (cons (cond-> {:db/ident (field-attr ns-prefix object-key key)
                             :db/doc label
                             :db/valueType (get datomic-types type)
                             :db/cardinality cardinality
                             (field-ident ns-prefix "name") name
                             (field-ident ns-prefix "type") type}
                      ;; Sorta funny that id types don't have :unique true
                      (or (= "id" type) unique)
                      (assoc :db/unique :db.unique/identity)
                      calculatedFormula
                      (assoc (field-ident ns-prefix "formula") calculatedFormula)
                      inlineHelpText
                      (assoc (field-ident ns-prefix "helptext") inlineHelpText))
                    (when (case type
                            "picklist" true
                            "multipicklist" true
                            false)
                      (enum-datoms key field)))))]
    (into [] (mapcat field-datoms) fields)))

(defn build-schema!
  [client ns-prefix object-keys]
  [(metadata-schema ns-prefix)
   (into []
         (mapcat (fn [object-key]
                   (object-schema ns-prefix object-key (c/get-fields client object-key))))
         object-keys)])

(defn assert-object!
  [client ns-prefix object-key m]
  (let [fields (c/get-fields client object-key)]
    (reduce-kv (fn [txn field-key value]
                 (let [attr (field-attr ns-prefix object-key field-key)
                       field (get fields field-key)
                       {:keys [type referenceTo]} field
                       [value ref-types]
                       (case type
                         "picklist"
                         (enum-ident ns-prefix object-key field-key (picklist-name value))
                         "multipicklist"
                         [(into []
                                (map (comp (partial enum-ident ns-prefix object-key field-key)
                                           picklist-name))
                                (s/split value #";"))]
                         "date"
                         [(tc/to-date value)]
                         "reference"
                         (let [ref-key (clj/field->refers-attr field)
                               ref-object (assert-object! client ns-prefix ref-key value)]
                           [(dissoc ref-object ::types)
                            (get ref-object ::types)])
                         [value])]
                   (-> txn
                       (assoc attr value)
                       (cond-> (seq ref-types)
                         (update ::types into ref-types)))))
               {::types #{object-key}}
               m)))

(defn assert-query!
  "Returns a seq of transaction seqs that if transacted in order will assert
   the results of the given query in a datomic database.

   Given an ns-prefix of `ex` and a query of `{:find [:customer :id :sectors]}`

   The first transaction asserts a set of attributes that will be defined on the
   attributes that will model the salesforce fields where there is no direct
   datomic analog, e.g. `ex.field/helptext`.

   The second transaction asserts a set of attributes that model the
   fields of objects used in the query,
   e.g. `ex.object.customer/id`. Note this is a complete set of
   attributes, not limited simply to those used in the query.

   The last transaction asserts the entities returned by the query.

   Most field values have natural datomic types. Notably, however, picklist
   and multipicklist fields are modeled as enums whose idents are a
   keywordized version of the picklist value with a namespace derived from
   the object and field, e.g. `ex.object.customer.sectors/energy-efficiency`."
  [client ns-prefix query]
  (let [objects (into []
                      (comp (map (comp first seq))
                            (map (partial apply assert-object! client ns-prefix)))
                      (q/query client query))
        object-keys (reduce set/union #{} (map ::types objects))]
    (conj (build-schema! client ns-prefix object-keys)
          (into [] (map (fn [m] (dissoc m ::types)) objects)))))
