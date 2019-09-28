(ns sails-forth.query
  "Provides for executing queries using more idiomatic clojure forms"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :as sc]
            [sails-forth.spec :as spec]))

(defprotocol SoqlValue
  (soql-value [_]))

(defn soql-string-escape
  [s]
  (-> s
    (string/replace #"\\" (string/re-quote-replacement "\\\\"))
    (string/replace #"\n" (string/re-quote-replacement "\\n"))
    (string/replace #"'" (string/re-quote-replacement "\\'"))))

;; TODO figure out how to annotate these impls
(extend-protocol SoqlValue
  String
  (soql-value [s]
    (str "'" (soql-string-escape s) "'"))
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
                   fields'))))))
  org.joda.time.DateTime
  (soql-value [dt]
    (.toString dt))
  org.joda.time.LocalDate
  (soql-value [ld]
    (.toString ld))
  nil
  (soql-value [_] "null"))

(s/def ::where-operator
  #{:in := :or :> :< :>= :<=})

(s/def ::where-value
  (s/or :string string?
        :set (s/coll-of string? :kind set?)
        :fields (s/coll-of (s/or :simple-field ::spec/field
                                 :field-description ::spec/field-description)
                           :kind vector?)))

;; use tuple over cat so `exercise` will generate vectors.
(s/def ::where-clause
  (s/tuple #_:operator ::where-operator
           #_:lhs ::where-value
           #_:rhs ::where-value))

(declare soql-where*)

(s/fdef soql-where
  :args (s/cat :clause ::where-clause)
  :ret string?)

(defn ^:spark/no-boot-spec-coverage soql-where
  [[op & args]]
  ;; TODO the type of op is significant
  (case op
    :or (str "(" (string/join ") OR (" (map soql-where* args)) ")")
    (let [[lh rh] args]
      (str (soql-value lh) " " (name op) " " (soql-value rh)))))

(s/fdef soql-where*
  :args (s/cat :clauses (s/coll-of ::where-clause))
  :ret string?)

(defn ^:spark/no-boot-spec-coverage soql-where*
  [where*]
  (string/join " AND " (map soql-where where*)))

(s/fdef soql-query
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :field-paths (s/coll-of ::sc/field-path :min-count 1)
               :where (s/coll-of ::where-clause))
  :ret string?)

(defn soql-query
  "Creates a soql query string for the given client, type, and seq of field
   paths"
  [client type field-paths where]
  (let [description (sf/get-type-description client type)
        soql-fields (map soql-value field-paths)]
    (str "SELECT " (string/join "," soql-fields)
         " FROM " (:name description)
         (when (seq where)
           (str " WHERE " (soql-where* where))))))

(defn update-attr-path
  [client where]
  (mapv (fn [[op & args :as clause]]
          (case op
            :or `[:or ~@(map (partial update-attr-path client) args)]
            (update clause 1 (fn [path] (sf/resolve-attr-path client (nth path 0) (subvec path 1))))))
        where))

(s/fdef query-attr-paths
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :attr-paths (s/coll-of ::sc/attr-path :min-count 1)
               :where (s/nilable (s/coll-of ::where-clause)))
  :ret (s/coll-of map? :kind vector?))

(defn query-attr-paths
  "Queries the given client and type for the given seq of attr-paths, e.g.
   [[:account :name] [:account :createdby :lastname]]. This returns a vector
   of maps with keyword paths matching each of the attr-paths, e.g.
   {:account {:name ... :createdby {:lastname ...}}}. The base type will also
   have metadata with a :url resolvable by the current client."
  [client type attr-paths where]
  (let [field-paths (mapv (partial sf/resolve-attr-path client type) attr-paths)
        where (update-attr-path client where)
        soql (soql-query client type field-paths where)
        records (sf/query! client soql)]
    (mapv (fn [record]
            (let [url (get-in record [:attributes :url])]
              (with-meta
                (reduce (fn [record' [field-path attr-path]]
                          (let [record-path (sf/resolve-field-path field-path)
                                value (get-in record record-path)
                                field (last field-path)]
                            (cond-> record'
                              value
                              (assoc-in attr-path (sc/default-coerce-from-salesforce field value)))))
                        {}
                        (map vector field-paths attr-paths))
                {:url url})))
          records)))

(s/def ::variant
  (s/coll-of (s/or :attr ::sc/attr :variant ::variant)
             :kind vector? :min-count 1))

(s/fdef expand-variants
  :args (s/cat :variant ::variant)
  :ret (s/coll-of ::sc/attr-path :kind vector?))

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

(s/def ::find
  ::variant)

(s/def ::where
  (s/coll-of ::where-clause :kind vector?))

(s/def ::query
  (s/keys :req-un [::find]
          :opt-un [::where]))

(s/fdef query
  :args (s/cat :client ::sf/client
               :query ::query)
  :ret (s/coll-of map? :kind vector?)
  ;; TODO :fn specify structure of maps from query find
  )

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
              (query-attr-paths client type (map #(subvec % 1) attr-paths) (:where query)))))))

(s/fdef record-types
  :args (s/cat :client ::sf/client)
  :ret (s/map-of ::spec/id string?))

(defn ^:spark/no-boot-spec-coverage record-types
  [client]
  (let [cache (sf/cache client)]
    (or (sf/get! cache ::record-types)
        (let [rows (query client {:find [:recordtype :id :name]})
              idx (->> (map :recordtype rows)
                       (map (juxt :name :id))
                       (into {}))]
          (sf/put! cache ::record-types idx)
          idx))))

(s/fdef record-type-id
  :args (s/cat :client ::sf/client
               :name ::spec/type)
  :ret ::spec/id)

(defn ^:spark/no-boot-spec-coverage record-type-id
  [client record-type-name]
  (get (record-types client) record-type-name))
