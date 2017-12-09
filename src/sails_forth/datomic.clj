(ns sails-forth.datomic
  (:require [sails-forth.client :as c]))

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
   ;; TODO enum? But how can it be constrained?
   "picklist" :db.type/string
   "url" :db.type/uri
   ;; TODO enum? Same as picklist but with cardinality many
   "multipicklist" :db.type/string
   ;; TODO component? Does this have discrete pieces?
   "address" :db.type/string
   ;; TODO component? Same as address
   "phone" :db.type/string})

(defn datomic-schema
  [client ns-prefix type fields]
  (let [ns-prefix (name ns-prefix)
        ns (format "%s.%s" ns-prefix (name type))
        name-md (keyword ns-prefix "name")
        formula-md (keyword ns-prefix "formula")
        helptext-md (keyword ns-prefix "helptext")
        ;; TODO there is so much other salesforce schema metadata
        md [{:db/ident name-md
             :db/doc "Salesforce field name"
             :db/valueType :db.type/string
             :db/cardinality :db.cardinality/one}
            {:db/ident formula-md
             :db/doc "Salesforce field formula"
             :db/valueType :db.type/string
             :db.cardinality/one :db.cardinality/one}
            {:db/ident helptext-md
             :db/doc "Salesforce help text"
             :db/valueType :db.type/string
             :db.cardinality/one :db.cardinality/one}]]
    (into md (map (fn [key field]
                    (let [{:keys [name
                                  label
                                  type
                                  calculatedFormula
                                  inlineHelpText
                                  unique]}
                          field]
                      (cond-> {:db/ident (keyword ns key)
                               :db/doc label
                               :db/valueType (get datomic-types type)
                               :db/cardinality :db.cardinality/one
                               name-md name}
                        ;; Sorta funny that id types don't have :unique true
                        (or (= "id" type) unique)
                        (assoc :db/unique :db.unique/identity)
                        calculatedFormula
                        (assoc formula-md calculatedFormula)
                        inlineHelpText
                        (assoc helptext-md inlineHelpText)))))
          (c/get-fields client type))))

(comment
  {:find [:contact :id :name [:phone :id :number]]}

  (datomic-schema client :sparkfund.salesforce :contact [:id :name]) ; =>

  [{:db/ident :sparkfund.salesforce.contact/id
    :db/valueType :db.type/string
    :db/doc "..."
    :db.install/_attribute :db.part/db
    ;; :db.unique :db.unique/value ;; :db.unique/identity
    ;; :db.index true
    ;; :db.component
    }
   {:db/ident :sparkfund.salesforce.contact/name
    :db/valueType :db.type/string
    :db/doc "..."
    :db.install/_attribute :db.part/db}
   ]

  (datomic-schema client :sparkfund.salesforce :phone [:id :name])

  [{:db/id "contact"
    :contact/id ...
    :contact/name ...
    :contact/phone {:phone/id ...
                    :phone/number ...}}]

  )
