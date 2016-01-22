(ns sails-forth.query
  (:require [clj-time.format :as tf]
            [clojure.set :as set]
            [clojure.string :as string]
            [sails-forth :as sf]))

(defn build-soql
  [type fields]
  (str "SELECT " (string/join "," (map name fields))
       " FROM " (name type)))

(defn field->attr
  [s]
  (-> s
      (string/replace #"__c\Z" "")
      string/lower-case
      (string/replace \_ \-)
      keyword))

(defn get-type-description
  [client type]
  (if-let [description (get-in @client [::types type])]
    description
    (let [description (sf/describe! client (name type))
          {:keys [fields]} description
          field-index (->> fields
                           (map (juxt :name identity))
                           (into {}))
          attr->field (->> fields
                           (map :name)
                           (map (juxt field->attr identity))
                           (into {}))
          description (assoc description
                             ::field-index field-index
                             ::attr->field attr->field
                             ::field->attr (set/map-invert attr->field))]
      (swap! client assoc-in [::type type] description)
      description)))

(defn get-field-description
  [client type attr]
  (let [type-description (get-type-description client type)
        field (get-in type-description [::attr->field attr])]
    (get-in type-description [::field-index field])))

(def parse-value
  (let [date-formatter (tf/formatters :date-time)]
    (fn [type value]
      (case type
        "datetime" (tf/parse date-formatter value)
        value))))

(defn query
  [client type attrs]
  (let [description (get-type-description client type)
        attr->field (::attr->field description)
        field->attr (::field->attr description)
        field-index (::field-index description)
        fields (map attr->field attrs)
        field->type (->> fields
                         (map field-index)
                         (map (juxt :name :type))
                         (into {}))
        soql (build-soql type fields)
        records (sf/query! client soql)]
    (mapv (fn [record]
            (reduce-kv (fn [accum field value]
                         (let [attr (get field->attr (name field) field)
                               type (get field->type (name field))
                               value (parse-value type value)]
                           (assoc accum attr value)))
                       {}
                       record))
          records)))
