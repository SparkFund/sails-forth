(ns sails-forth.memory
  (:require [clojure.set :as set]
            [clojure.string :as string])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [org.joda.time DateTime LocalDate])
  (:refer-clojure :exclude [update list]))

(defn build-state
  [schema]
  {:last-id 0
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

(defn validate-existence
  [state type id]
  (let [{:keys [objects]} state]
    (when-not (get-in objects [type id])
      (throw (ex-info "object not found" {:type type :id id})))))

(defn validate-attr
  [state type value]
  (case type
    "string"
    (string? value)
    "datetime"
    (instance? DateTime value)
    "date"
    (instance? LocalDate value)
    "double"
    (number? value)
    "int"
    (integer? value)
    (let [{:keys [objects]} state]
      (contains? (objects type) value))))

(defn validate-attrs
  [state type attrs]
  (let [{:keys [schema]} state
        type-schema (type-schema schema type)]
    (when (or (seq (set/difference (set (keys attrs)) (set (keys type-schema))))
              (not (every? (fn [[attr value]]
                             (validate-attr state (type-schema attr) value))
                           attrs)))
      (throw (ex-info "invalid attrs" {:attrs attrs})))))

(defn create
  [state type attrs]
  (let [{:keys [schema objects last-id]} state
        id (inc last-id)]
    (validate-attrs state type attrs)
    (-> state
        (assoc :last-id id)
        (assoc-in [:objects type id] attrs))))

(defn create!
  [astate stype attrs]
  (:last-id (swap! astate create (keyword stype) attrs)))

(defn delete
  [state type id]
  (validate-existence state type id)
  (update-in state [:objects type] dissoc id))

(defn delete!
  [astate stype id]
  (swap! astate delete (keyword stype) id)
  true)

(defn update
  [state type id attrs]
  (validate-existence state type id)
  (let [old (get-in state [:objects type id])
        new (merge old attrs)]
    (validate-attrs state type new)
    (assoc-in state [:objects type id] attrs)))

(defn update!
  [astate stype id attrs]
  (swap! astate update (keyword stype) id attrs)
  true)

(defn list
  [state type]
  (->> (get-in state [:objects type])
       (map (fn [[id attrs]]
              (assoc attrs :id id)))
       (into [])))

(defn list!
  [astate stype]
  (list @astate (keyword stype)))

(defn describe
  [state type]
  (let [{:keys [schema]} state
        type-schema (assoc (type-schema schema type)
                           :id "string")]
    {:name (name type)
     :fields (mapv (fn [[attr type]]
                     (let [type' (if (string? type)
                                   type
                                   "reference")]
                       (cond-> {:name (name attr)
                                :label (name attr)
                                :type type'}
                         (= type' "reference")
                         (assoc :referenceTo [(name type)]
                                :relationshipName (name type)))))
                   type-schema)}))

(defn describe!
  [astate stype]
  (describe @astate (keyword stype)))

(defn objects
  [state]
  (let [{:keys [schema]} state]
    {:sobjects (mapv (fn [type]
                       {:name (name type)})
                     (keys schema))}))

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
  (render [column object]
    ;; TODO this assumes all where paths are found in the select clause
    (let [path (mapv keyword (string/split (str column) #"\."))]
      (get-in object path)))
  net.sf.jsqlparser.expression.LongValue
  (render [value _]
    (.getValue value))
  net.sf.jsqlparser.expression.StringValue
  (render [value _]
    (.getValue value)))

(defn parse-soql
  [soql]
  (let [ps (.getSelectBody (CCJSqlParserUtil/parse soql))
        kws (comp keyword str)]
    {:select (->> (.getSelectItems ps)
                  (map str)
                  (map #(string/split % #"\."))
                  (mapv (partial mapv keyword)))
     :from (keyword (str (.getFromItem ps)))
     :where (partial allows? (.getWhere ps))}))

(defn project
  [state type object path]
  (let [{:keys [schema objects]} state
        type-schema (assoc (type-schema schema type) :id "string")
        path-type (type-schema (first path))]
    (when-not path-type
      (throw (ex-info "invalid path" {:type type :path path})))
    (if (keyword? path-type)
      (do
        (when-not (next path)
          (throw (ex-info "invalid path" {:type type :path path})))
        (if-let [id (get object (first path))]
          (let [object' (assoc (get-in objects [path-type id]) :id id)]
            {(first path)
             (project state path-type object' (next path))})
          {}))
      (do
        (when (next path)
          (throw (ex-info "invalid path" {:type type :path path})))
        (select-keys object [(first path)])))))

(defn deep-merge
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn query
  [state soql]
  (let [{:keys [schema objects]} state
        {:keys [select from where]} (parse-soql soql)]
    (->> (map (fn [[id attrs]] (assoc attrs :id id)) (objects from))
         (map (fn [object]
                (reduce deep-merge {} (map (partial project state from object) select))))
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
