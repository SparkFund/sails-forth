(ns sails-forth.memory2
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [sails-forth.clojurify :as clj])
  (:import [java.util UUID]
           [org.mule.tools.soql SOQLParserHelper]
           [org.joda.time DateTime LocalDate])
  (:refer-clojure :exclude [list update]))

;; TODO could narrow this down a bit lol
(s/def ::schema
  any?)

(defn build-state
  [schema]
  {:last-id nil
   :objects {}
   :schema schema})

(defn create-state!
  [schema]
  (atom (build-state schema)))

(defn type-schema
  [schema type]
  (let [type-schema (schema type)]
    (when-not type-schema
      (throw (ex-info "invalid type" {:type type})))
    type-schema))

(defn object-exists?
  [state type id]
  (let [{:keys [objects]} state]
    (get-in objects [type id])))

(defn validate-existence
  [state type id]
  (when-not (object-exists? state type id)
    (throw (ex-info "object not found" {:type type :id id}))))

(defn validate-attr
  [state field value]
  (let [{:keys [type]} field
        picklist-values (->> (:picklistValues field)
                             (filter :active)
                             (map :value)
                             (into #{}))]
    (or (nil? value)
        (case type
          "id"
          (string? value)
          "string"
          (string? value)
          "textarea"
          (string? value)
          "url"
          (string? value)
          "datetime"
          (instance? DateTime value)
          "date"
          (instance? LocalDate value)
          "double"
          (number? value)
          "currency"
          (number? value)
          "percent"
          (number? value)
          "int"
          (integer? value)
          "picklist"
          (contains? picklist-values value)
          "multipicklist"
          (let [all-values (string/split value #";")]
            (every? (partial contains? picklist-values) all-values))
          "phone"
          (string? value)
          "boolean"
          (#{true false} value)
          "reference"
          (let [type (first (:referenceTo field))]
            (object-exists? state type value))
          (throw (ex-info "Unknown field type" {:field field}))))))

(defn validate-attrs
  [state type attrs]
  (let [{:keys [schema]} state
        type-schema (type-schema schema type)
        {:keys [fields]} type-schema
        fields (into {} (map (juxt :name identity) fields))
        diff (set/difference (set (keys attrs)) (set (keys fields)))
        invalid-attrs (keep (fn [[attr value]]
                              (let [field (get fields attr)
                                    value (clj/default-coerce-from-salesforce field value)]
                                nil))
                            attrs)]
    (when (or (seq diff) (not (empty? invalid-attrs)))
      (throw (ex-info "invalid attrs"
                      {:attrs attrs
                       :extra-keys diff
                       :values-with-invalid-type invalid-attrs})))))

(defn create
  [state type attrs]
  (let [{:keys [schema objects]} state
        id (str (UUID/randomUUID))
        attrs' (assoc attrs "Id" id)]
    (validate-attrs state type attrs')
    (-> state
        (assoc :last-id id)
        (assoc-in [:objects type id] attrs'))))

(defn create!
  [astate type attrs]
  (:last-id (swap! astate create type attrs)))

(defn delete
  [state type id]
  (validate-existence state type id)
  (update-in state [:objects type] dissoc id))

(defn delete!
  [astate type id]
  (swap! astate delete type id)
  true)

(defn update-object
  [state type id attrs]
  (validate-existence state type id)
  (let [old (get-in state [:objects type id])
        new (merge old attrs)]
    (validate-attrs state type new)
    (assoc-in state [:objects type id] new)))

(defn update!
  [astate type id attrs]
  (swap! astate update-object type id attrs)
  true)

(defn list
  [state type]
  (into [] (vals (get-in state [:objects type]))))

(defn list!
  [astate type]
  (list @astate type))

(defn describe
  [state type]
  (let [{:keys [schema]} state]
    (type-schema schema type)))

(defn describe!
  [astate stype]
  (describe @astate stype))

(defn objects
  [state]
  (let [{:keys [schema]} state]
    {:sobjects (mapv #(dissoc % :fields) (vals schema))}))

(defn objects!
  [astate]
  (objects @astate))

(defprotocol Filter
  (allows? [this schema object]
    "Returns true if the object is allowed by this filter"))

(defprotocol Evaluable
  ;; This is used by the filter implementations to evaluate their operands
  (eval2 [this schema object]
    "Evalutes this against the schema and object"))

(defn parse-literal
  [s]
  ;; Absolutely wild that the parser throws away the type info
  ;; so we get to duplicate the cases about which we care here
  (condp re-matches s
    #"true" true
    #"false" false
    #"null" nil
    #"\d\d\d\d-\d\d-\d\d" (org.joda.time.LocalDate. s)
    #"\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?(Z|(\+|-)?\d\d:\d\d)?" (org.joda.time.DateTime. s)
    #"(\+|-)?\d+" (Long/parseLong s)
    #"(\+|-)?\d+(\.\d+)?" (Double/parseDouble s)
    #"'(.*)'" :>> (fn [[_ s]] s)))

(extend-protocol Evaluable
  org.mule.tools.soql.query.data.Literal
  (eval2 [literal _ _]
    (parse-literal (.toString literal)))
  org.mule.tools.soql.query.data.Field
  (eval2 [field schema object]
    (let [object-path (into [] (map keyword) (.getObjectPrefixNames field))
          object (cond-> object
                   (seq object-path)
                   (get-in object-path))
          type (get-in object [:attributes :type])
          field-name (.getFieldName field)
          sf-field (some (fn [sf-field]
                           (when (= field-name
                                    (get sf-field :name))
                             sf-field))
                         (get-in schema [type :fields]))
          value (get object (keyword field-name))]
      (when value
        (clj/default-coerce-from-salesforce sf-field value)))))

(extend-protocol Filter
  org.mule.tools.soql.query.condition.operator.AndOperator
  (allows? [operator schema object]
    (and (allows? (.getLeftCondition operator) schema object)
         (allows? (.getRightCondition operator) schema object)))
  org.mule.tools.soql.query.condition.operator.OrOperator
  (allows? [operator schema object]
    (or (allows? (.getLeftCondition operator) schema object)
        (allows? (.getRightCondition operator) schema object)))
  org.mule.tools.soql.query.condition.operator.NotOperator
  (allows? [operator schema object]
    (not (allows? (.getCondition operator) schema object)))
  org.mule.tools.soql.query.condition.operator.Parenthesis
  (allows? [operator schema object]
    (allows? (.getCondition operator) schema object))
  org.mule.tools.soql.query.condition.FieldBasedCondition
  (allows? [condition schema object]
    (let [pred (case (.toString (.getOperator condition))
                 ">" pos?
                 ">=" (complement neg?)
                 "<" neg?
                 "<=" (complement pos?)
                 "=" zero?
                 "!=" (complement zero?)
                 "<>" (throw (ex-info "Not implemented" {})))]
      (pred (compare (eval2 (.getConditionField condition) schema object)
                     (eval2 (.getLiteral condition) schema object)))))
  org.mule.tools.soql.query.condition.SetBasedCondition
  (allows? [condition schema object]
    (let [values (into #{}
                       (map (fn [literal]
                              (eval2 literal schema object)))
                       (.getValues (.getSet condition)))
          value (eval2 (.getConditionField condition) schema object)
          set-contains-value? (contains? values value)]
      (case (.toString (.getOperator condition))
        "IN"
        set-contains-value?
        "NOT IN"
        (not set-contains-value?))))
  nil
  (allows? [_ _ _]
    true))

(defn parse-soql
  [soql]
  (let [form (SOQLParserHelper/createSOQLData soql)]
    ;; TODO could validate the other bits of the soql query are missing
    {:select (for [spec (.getSelectSpecs form)]
               (conj (into [] (.getObjectPrefixNames spec)) (.getFieldName spec)))
     :from (.toString (.getMainObjectSpec (.getFromClause form)))
     :where (some-> (.getWhereClause form) (.getCondition))}))

(defn project
  [state type object path]
  (let [{:keys [schema objects]} state
        type-schema (type-schema schema type)
        fields (->> (:fields type-schema)
                    (map (fn [field]
                           (let [k (if (= "reference" (:type field))
                                     (:relationshipName field)
                                     (:name field))]
                             [k field])))
                    (into {}))
        path-field (get fields (first path))]
    (when-not path-field
      (throw (ex-info "invalid path" {:type type :path path})))
    (if (= "reference" (:type path-field))
      (do
        (when-not (next path)
          (throw (ex-info "invalid path" {:type type :path path})))
        (if-let [id (get object (:name path-field))]
          (let [type' (first (:referenceTo path-field))
                object' (get-in state [:objects type' id])]
            {(keyword (first path))
             (project state (first (:referenceTo path-field)) object' (next path))})
          {}))
      (do
        (when (next path)
          (throw (ex-info "invalid path" {:type type :path path})))
        (-> object
            (select-keys [(first path)])
            walk/keywordize-keys
            (assoc :attributes {:type type}))))))

(defn deep-merge
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn query
  [state soql]
  (let [{:keys [schema objects]} state
        {:keys [select from where]} (parse-soql soql)]
    (->> (vals (get objects from))
         (map (fn [object]
                (reduce deep-merge {}
                        (map (partial project state from object) select))))
         (filter (partial allows? where schema))
         (into []))))

(defn query!
  [astate soql]
  (query @astate soql))

(defn count!
  [astate soql]
  (count (query! astate soql)))

(defn limits!
  [astate]
  {})

(defn take-action!
  [astate take-action-map action inputs]
  (if-let [f (get take-action-map action)]
    (f astate inputs)
    (throw (ex-info "action failed" {:cause (str action " not implemented")
                                     :take-action-map take-action-map}))))
