(ns sails-forth.clojurify
  "Translates between SalesForce string API type/attribute names and
  more Clojure-y keywords."
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [sails-forth.spec :as spec])
  (:import [org.joda.time DateTime LocalDate]))

(s/def ::attr
  keyword?)

(s/def ::attr-path
  (s/coll-of ::attr :kind vector? :min-count 1))

(s/def ::field-path
  (s/coll-of ::spec/field-description :kind vector? :min-count 1))

(s/fdef field->attr
  :args (s/cat :field ::spec/field-description)
  :ret ::attr)

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

(s/fdef field->refers-attr
  :args (s/cat :field ::spec/field-description)
  :ret ::attr)

(defn ^:spark/no-boot-spec-coverage field->refers-attr
  "Derives a clojurey attribute keyword representation of the Salesforce
   relation about which this field refers"
  [field]
  (let [{:keys [referenceTo type]} field]
    (when (or (not= "reference" type)
              (not (and (= 1 (count referenceTo))
                        (string? (first referenceTo)))))
      (throw (ex-info "Invalid reference field" {:field field})))
    (-> referenceTo
        first
        (string/replace #"__c\Z" "")
        string/lower-case
        (string/replace \_ \-)
        keyword)))

(s/fdef object->attr
  :args (s/cat :object ::spec/object-overview)
  :ret ::attr)

(defn object->attr
  "Derives a clojurey type keyword representation of a Salesforce object.
   This converts snake case to kebob case and removes any custom field suffix."
  [object]
  (let [{:keys [name custom]} object
        name' (string/replace name #"__c\Z" "")]
    (-> name'
        string/lower-case
        (string/replace #"_{1,2}" "-")
        keyword)))

(s/fdef set-map
  :args (s/cat :entries
               (s/and (s/coll-of (s/tuple keyword? any?))
                      #(= (count %) (count (set (map first %))))))
  :ret (s/map-of keyword? any?))

(defn set-map
  "Builds a map from the given seq of entries, raising on any duplicate key"
  [entries]
  (reduce (fn [accum [k v]]
            (when (contains? accum k)
              (let [msg "Duplicate key given for map"]
                (throw (ex-info msg {:key k :entries entries}))))
            (assoc accum k v))
          {}
          entries))

(s/def ::datetime
  (partial instance? DateTime))

(s/def ::date
  (partial instance? LocalDate))

(s/def ::value
  (s/or :datetime ::datetime
        :date ::date
        :int integer?
        :bigdec decimal?
        :other ::spec/json-simple))

(s/fdef parse-value
  :args (s/cat :field ::spec/field-description
               :value ::spec/json-simple)
  :ret ::value
  ;; TODO could specify that render-value is the inverse
  )

(def double-type
  (let [max-long-precision (dec (count (str Long/MAX_VALUE)))]
    (fn [field]
      (let [{:keys [scale precision]} field]
        (if (zero? scale)
          (if (<= precision max-long-precision)
            :long
            :bigint)
          :bigdec)))))

(def parse-value
  "Parses the given value according to its field type and other characteristics"
  (let [date-time-formatter (tf/formatters :date-time)
        date-formatter (tf/formatters :date)
        max-long-precision (dec (count (str Long/MAX_VALUE)))]
    (fn [field value]
      (let [{:keys [type scale precision]} field]
        (case type
          "datetime" (tf/parse date-time-formatter value)
          "date" (tc/to-local-date (tf/parse date-formatter value))
          "double" ((case (double-type field)
                      :long long
                      :bigint bigint
                      :bigdec bigdec) value)
          "int" (long value)
          "percent" (/ value 100M)
          value)))))

(defmulti default-coerce-from-salesforce
  (fn [field value]
    (:type field)))

(defmethod default-coerce-from-salesforce :default
  [field value]
  (parse-value field value))

(s/fdef render-value
  :args (s/cat :field ::spec/field-description
               :value ::value)
  :ret ::spec/json-simple
  ;; TODO could specify that parse-value is the inverse
  )

(def render-value
  "Parses the given value according to its field type and other characteristics"
  (let [date-time-formatter (tf/formatters :date-time)
        date-formatter (tf/formatters :date)]
    (fn [field value]
      (let [{:keys [type scale precision]} field]
        (case type
          "datetime" (tf/unparse date-time-formatter (tc/to-date-time value))
          "date" (tf/unparse date-formatter (tc/to-date-time value))
          "percent" (* value 100M)
          value)))))

(defmulti default-coerce-to-salesforce
  (fn [field value]
    (:type field)))

(defmethod default-coerce-to-salesforce :default
  [field value]
  (render-value field value))
