(ns sails-forth.datomic
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
   ;; TODO component? Does this have discrete pieces?
   ;; "address" :db.type/string
   ;; TODO component? Same as address
   ;; "phone" :db.type/string
   })

(defn metadata-schema
  [ns-prefix]
  ;; TODO there are so many other salesforce schema metadata
  (let [ns-prefix (name ns-prefix)
        name-md (keyword ns-prefix "name")
        type-md (keyword ns-prefix "type")
        formula-md (keyword ns-prefix "formula")
        helptext-md (keyword ns-prefix "helptext")
        picklist-value-md (keyword ns-prefix "picklist-value")]
    [{:db/ident name-md
      :db/doc "Salesforce field name"
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident type-md
      :db/doc "Salesforce field type"
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident formula-md
      :db/doc "Salesforce field formula"
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident helptext-md
      :db/doc "Salesforce help text"
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident picklist-value-md
      :db/doc "Salesforce picklist value"
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}]))

(defn object-schema
  [ns-prefix object-key fields]
  (let [ns-prefix (name ns-prefix)
        ns (str ns-prefix "." (name object-key))]
    (letfn [(enum-datoms [key field]
              (letfn [(enum-datom [item]
                        (let [ns (str ns "." (name key))
                              {:keys [label value]} item]
                          ;; TODO value may not become a keyword that can be
                          ;; read by the clj/edn reader. That's annoying.
                          ;; Whatever we choose to do here must be understood
                          ;; by our readers and writers
                          {:db/ident (keyword ns value)
                           ;; TODO are docstrings allowed on non-attributes?
                           :db/doc label
                           (keyword ns-prefix "picklist-value") value}))]
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
                (cons (cond-> {:db/ident (keyword ns (clojure.core/name key))
                               :db/doc label
                               :db/valueType (get datomic-types type)
                               :db/cardinality cardinality
                               (keyword ns-prefix "name") name
                               (keyword ns-prefix "type") type}
                        ;; Sorta funny that id types don't have :unique true
                        (or (= "id" type) unique)
                        (assoc :db/unique :db.unique/identity)
                        calculatedFormula
                        (assoc (keyword ns-prefix "formula") calculatedFormula)
                        inlineHelpText
                        (assoc (keyword ns-prefix "helptext") inlineHelpText))
                      (when (case type
                              "picklist" true
                              "multipicklist" true
                              false)
                        (enum-datoms key field)))))]
      (into [] (mapcat field-datoms) fields))))

(defn build-schema!
  [client ns-prefix object-keys]
  (into (metadata-schema ns-prefix)
        (mapcat (fn [object-key]
                  (object-schema ns-prefix object-key (c/get-fields client object-key))))
        object-keys))

(defn assert-object!
  [client ns-prefix object-key m]
  (let [ns-prefix (name ns-prefix)
        ns (str ns-prefix "." (name object-key))
        fields (c/get-fields client object-key)]
    (reduce-kv (fn [txn field-key value]
                 (let [attr (keyword ns (name field-key))
                       field (get fields field-key)
                       {:keys [type referenceTo]} field
                       [value ref-types]
                       (case type
                         ;; changes must be coordinated with enum-datom
                         "picklist"
                         [(keyword (str ns "." (name field-key)) value)]
                         "multipicklist"
                         [(mapv (partial keyword (str ns "." (name field-key)))
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

;; TODO also do we want the whole schema for each type or just projections that
;; permit out query results
(defn assert-query!
  [client ns-prefix query]
  (let [objects (into []
                      (comp (map (comp first seq))
                            (map (partial apply assert-object! client ns-prefix)))
                      (q/query client query))
        object-keys (reduce set/union #{} (map ::types objects))]
    (into (build-schema! client ns-prefix object-keys)
          (map (fn [m] (dissoc m ::types))
               objects))))
