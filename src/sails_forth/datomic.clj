(ns sails-forth.datomic
  (:require [clj-time.coerce :as tc]
            [clojure.string :as s]
            [sails-forth.client :as c]
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
  [ns-prefix type fields]
  (let [ns-prefix (name ns-prefix)
        ns (str ns-prefix "." (name type))]
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
  [client ns-prefix types]
  (into (metadata-schema ns-prefix)
        (mapcat (fn [type]
                  (object-schema ns-prefix type (c/get-fields client type))))
        types))

(defn assert-object
  [ns-prefix type fields m]
  (let [ns-prefix (name ns-prefix)
        ns (str ns-prefix "." (name type))]
    (reduce-kv (fn [txn field value]
                 (let [attr (keyword ns (name field))
                       type (get-in fields [field :type])
                       value (case type
                               ;; TODO coordinate with enum-datom
                               "picklist"
                               (keyword (str ns "." (name field)) value)
                               "multipicklist"
                               (mapv (partial keyword (str ns "." (name field)))
                                     (s/split value #";"))
                               "date"
                               (tc/to-date value)
                               "reference"
                               ;; TODO here's the good bit do we just recurse?
                               ;; If so we need the the reference's type and
                               ;; fields
                               nil
                               value)]
                   (assoc txn attr value)))
               {}
               m)))

;; TODO the problem with doing this generally is that the name of an object's
;; reference is not always the same as the type, as in multiple references to
;; accounts from an opportunity under different names. Those data are available
;; we just need to figure out how to get at them sensibly.
;; TODO also do we want the whole schema for each type or just projections that
;; permit out query results
(defn assert-query!
  [client ns-prefix query])

(comment
  ;; TODO we could add metadata to the query results with the type of each query
  ;; path, e.g.
  ^{:path-types {[:opportunity] :opportunity
                 [:opportunity :customer-account] :account}}
  [{:opportunity [:customer-account {:name "Rey"}]}]
  ;; This could also be the job of some local coordinator, but that ties a good
  ;; bit of functionality to an impure fn
  )
