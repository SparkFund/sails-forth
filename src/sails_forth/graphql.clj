(ns sails-forth.graphql)

(def default-custom-scalars
  {:Date {:parse (fn parse-date [s]
                   (java.time.LocalDate/parse s))
          :serialize str}
   :DateTime {:parse (fn parse-datetime [s]
                       (java.time.ZonedDateTime/parse s))
              :serialize str}
   :Money {:parse (fn parse-money [s]
                    (.setScale ^BigDecimal (bigdec s) 2))
           :serialize str}})

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

(defn add-field
  [schema object field]
  (let [{:keys [deprecatedAndHidden name label inlineHelpText referenceTo type]} field
        description (or inlineHelpText label)
        picklist? (case type "multipicklist" true "picklist" true false)
        picklist-type (when picklist? (picklist-enum-type object field))
        gql-type (case type
                   "address" 'String ; TODO
                   "boolean" 'Boolean
                   "currency" :Money
                   "date" :Date
                   "datetime" :DateTime
                   "double" 'Float
                   "email" 'String ; TODO
                   "encryptedstring" 'String
                   "id" 'ID
                   "int" 'Int
                   "multipicklist" (list 'list picklist-type)
                   "percent" 'Float
                   "phone" 'String ; TODO
                   "picklist" (picklist-enum-type object field)
                   "reference" (convert-name (first referenceTo))
                   "string" 'String
                   "textarea" 'String
                   "url" 'String)
        gql-field (cond-> {:type gql-type}
                    description
                    (assoc :description description)
                    deprecatedAndHidden
                    (assoc :deprecated true))
        gql-field-path [:objects (convert-name (get object :name))
                        :fields (convert-name name)]]
    (cond-> (assoc-in schema gql-field-path gql-field)
      picklist?
      (assoc-in [:enums picklist-type] (build-picklist-enum field)))))

(defn add-object
  [schema object]
  (let [{:keys [name fields custom label]} object
        gql-object-path [:objects (convert-name name)]]
    (reduce (fn [schema field]
              (add-field schema object field))
            (assoc-in schema (conj gql-object-path :description) label)
            fields)))

(defn build-schema
  [objects]
  (reduce add-object
          {:scalars default-custom-scalars}
          objects))
