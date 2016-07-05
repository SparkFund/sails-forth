(ns sails-forth.query
  "Provides for executing queries using more idiomatic clojure forms"
  (:require [clojure.core.typed :as t]
            [clojure.string :as string]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :refer :all]))

(t/defprotocol SoqlValue
  (soql-value [_] :- t/Str))

;; TODO figure out how to annotate these impls
(extend-protocol SoqlValue
  String
  (soql-value [s]
    (str "'" (string/replace s #"'" "\\'") "'"))
  clojure.lang.IPersistentSet
  (soql-value [xs]
    (str "(" (string/join "," (map soql-value xs)) ")"))
  clojure.lang.IPersistentVector
  (soql-value [fields]
    (loop [refs []
           fields fields]
      (if-not (seq fields)
        (string/join "." refs)
        (let [field (first fields)
              fields' (next fields)]
          (when (and (not fields')
                     (= "reference" (:type field)))
            (throw (ex-info "Invalid field path"
                            {:field-path fields})))
          (let [ref (if (= "reference" (:type field))
                      (:relationshipName field)
                      (:name field))]
            (recur (conj refs ref)
                   fields')))))))

(t/defalias WhereOp
  "Operators allowed in where clauses"
  (t/U ':in ':= ':or))

(t/defalias WhereSimpleValue
  (t/U t/Str))

(t/defalias WhereValue
  (t/U WhereSimpleValue (t/Set WhereSimpleValue) FieldPath))

(t/defalias WhereClause
  '[WhereOp WhereValue WhereValue])

(declare soql-where*)

(t/ann ^:no-check soql-where
       [WhereClause -> t/Str])
(defn soql-where
  [[op & args]]
  ;; TODO the type of op is significant
  (case op
    :or (str "(" (string/join ") OR (" (map soql-where* args)) ")")
    (let [[lh rh] args]
      (str "(" (soql-value lh) " " (name op) " " (soql-value rh) ")"))))

(t/ann ^:no-check soql-where*
       [(t/Seqable WhereClause) -> t/Str])
(defn soql-where*
  [where*]
  (string/join " AND " (map soql-where where*)))

(t/ann ^:no-check soql-query
       [sf/SalesforceClient t/Keyword (t/NonEmptyVec FieldPath) (t/Vec WhereClause) -> t/Str])
(defn soql-query
  "Creates a soql query string for the given client, type, and seq of field
   paths"
  [client type field-paths where]
  (let [description (get-type-description client type)
        soql-fields (map soql-value field-paths)]
    (str "SELECT " (string/join "," soql-fields)
         " FROM " (:name description)
         (when (seq where)
           (str " WHERE " (soql-where* where))))))

(defn update-attr-path [client where]
  (mapv (fn [[op & args :as clause]]
          (case op
            :or `[:or ~@(map (partial update-attr-path client) args)]
            (update clause 1 (fn [path] (resolve-attr-path client (first path) (rest path))))))
        where))

(t/ann ^:no-check query-attr-paths
       [sf/SalesforceClient t/Keyword (t/NonEmptyVec AttrPath) (t/Vec WhereClause) -> (t/Vec t/Any)])
(defn query-attr-paths
  "Queries the given client and type for the given seq of attr-paths, e.g.
   [[:account :name] [:account :createdby :lastname]]. This returns a vector
   of maps with keyword paths matching each of the attr-paths, e.g.
   {:account {:name ... :createdby {:lastname ...}}}. The base type will also
   have metadata with a :url resolvable by the current client."
  [client type attr-paths where]
  (let [field-paths (mapv (partial resolve-attr-path client type) attr-paths)
        where (update-attr-path client where)
        soql (soql-query client type field-paths where)
        records (sf/query! client soql)]
    (mapv (fn [record]
            (let [url (get-in record [:attributes :url])]
              (with-meta
                (reduce (fn [record' [field-path attr-path]]
                          (let [record-path (resolve-field-path field-path)
                                value (get-in record record-path)
                                field (last field-path)]
                            (cond-> record'
                              value
                              (assoc-in attr-path (parse-value field value)))))
                        {}
                        (map vector field-paths attr-paths))
                {:url url})))
          records)))

(t/defalias Variant
  (t/Rec [v]
         (t/HVec [t/Keyword (t/U t/Keyword v) *])))

(t/ann ^:no-check expand-variants
       [Variant -> (t/Vec AttrPath)])
(defn expand-variants
  "Expands a variant path into a seq of attr paths"
  [variant-path]
  (let [[type & refs] variant-path]
    (reduce (fn [accum ref]
              (if (sequential? ref)
                (into accum (mapv (partial into [type]) (expand-variants ref)))
                (conj accum [type ref])))
            []
            refs)))

(t/defalias Query
  (t/HMap :mandatory {:find Variant}
          :optional {:where (t/Vec WhereClause)}
          :complete? true))

(t/ann ^:no-check query
       [sf/SalesforceClient Query -> (t/Vec t/Any)])
(defn query
  "Returns the results of the given query against the given client. The query is
   a map with a :find keyword whose value must be a vector of keywords and
   vectors; the first position of any vector is taken to be a reference type.
   For example, [:account :name [:createdby :lastname]]. The result will be a
   vector of maps whose structures are given by the find clause, e.g.
   {:account {:name ... :createdby {:lastname ...}}}"
  [client query]
  (let [attr-paths (expand-variants (:find query))]
    (when (seq attr-paths)
      (let [type (ffirst attr-paths)]
        (mapv (fn [record]
                {type record})
              (query-attr-paths client type (map next attr-paths) (:where query)))))))
