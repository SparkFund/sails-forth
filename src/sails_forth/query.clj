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
  [field]
  (let [{:keys [name type]} field
        name' (string/replace name #"__c\Z" "")
        custom? (not= name name')
        attrize #(-> string/lower-case (string/replace \_ \-) keyword)]
    (-> name'
        (cond->
          (and (= type "reference")
               (not custom?))
          (string/replace #"Id\Z" ""))
        string/lower-case
        (string/replace \_ \-)
        keyword)))

(defn get-type-description
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
      (swap! client assoc-in [::type type] description)
      description)))

(defn get-field-description
  [client type attr]
  (let [type-description (get-type-description client type)]
    (get-in type-description [::attr->field attr])))

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
        field-index (::field-index description)
        fields (map attr->field attrs)
        soql (build-soql type (map :name fields))
        records (sf/query! client soql)]
    (mapv (fn [record]
            (reduce-kv (fn [accum field-key value]
                         (let [field (get field-index field-key)
                               attr (if field (field->attr field) field-key)
                               value (if field (parse-value (:type field) value) value)]
                           (assoc accum attr value)))
                       {}
                       record))
          records)))
