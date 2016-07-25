(ns sails-forth.memory
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:import [java.util UUID]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [org.joda.time DateTime LocalDate])
  (:refer-clojure :exclude [update list]))

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
        fields (into {} (map (juxt :name identity) fields))]
    (when (or (seq (set/difference (set (keys attrs)) (set (keys fields))))
              (not (every? (fn [[attr value]]
                             (let [field (get fields attr)]
                               (validate-attr state field value)))
                           attrs)))
      (throw (ex-info "invalid attrs" {:attrs attrs})))))

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
  (allows? [_ object]))

(defprotocol Renderer
  (render [_ context]))

(extend-protocol Filter
  net.sf.jsqlparser.expression.operators.relational.EqualsTo
  (allows? [expr object]
    (= (render (.getLeftExpression expr) object)
       (render (.getRightExpression expr) object)))
  net.sf.jsqlparser.expression.operators.conditional.AndExpression
  (allows? [expr object]
    (and (allows? (.getLeftExpression expr) object)
         (allows? (.getRightExpression expr) object)))
  net.sf.jsqlparser.expression.operators.relational.InExpression
  (allows? [expr object]
    (let [items (->> (.getRightItemsList expr)
                     (.getExpressions)
                     (map #(render % object))
                     (into #{}))]
      (cond-> (boolean (items (render (.getLeftExpression expr) object)))
        (.isNot expr)
        (not))))
  nil
  (allows? [_ _]
    true))

(extend-protocol Renderer
  net.sf.jsqlparser.schema.Column
  (render [column projection]
    ;; TODO this assumes all where paths are found in the select clause
    (let [path (mapv keyword (string/split (str column) #"\."))]
      (get-in projection path)))
  net.sf.jsqlparser.expression.LongValue
  (render [value _]
    (.getValue value))
  net.sf.jsqlparser.expression.StringValue
  (render [value _]
    (.getValue value)))

(defn parse-soql
  [soql]
  (let [ps (.getSelectBody (CCJSqlParserUtil/parse soql))]
    {:select (->> (.getSelectItems ps)
                  (map str)
                  (mapv #(string/split % #"\.")))
     :from (str (.getFromItem ps))
     :where (partial allows? (.getWhere ps))}))

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
         (filter where)
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
