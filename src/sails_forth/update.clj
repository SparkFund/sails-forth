(ns sails-forth.update
  "Execute SalesForce updates using more idiomatic Clojure syntax."
  (:require [clojure.spec.alpha :as s]
            [sails-forth.client :as sf]
            [sails-forth.clojurify :as sc]
            [sails-forth.spec :as spec]))

(defn- get-sf-type-name
  [client type]
  (let [sf-type (sf/get-type-description client type)
        sf-type-name (:name sf-type)]
    (when-not sf-type-name
      (throw (ex-info (str "no SalesForce type for " type)
                      {:description sf-type})))
    sf-type-name))

(defn sf-attrs
  "Converts the given record to a map of salesforce attrs for the given object
   type"
  [client type record]
  (->> (for [[k v] record
             :let [desc (sf/get-field-description client type k)
                   sf-k (:name desc)]]
         (do (when-not desc
               (throw (ex-info (str "no SalesForce attribute for " k)
                               {:description desc})))
             [sf-k (sc/default-coerce-to-salesforce desc v)]))
       (into {})))

(s/fdef update!
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :object-id ::spec/id
               :values (s/map-of ::sc/attr ::sc/value))
  :ret #{true})

(defn ^:spark/no-boot-spec-coverage update!
  "Performs updates on the object `object-id` with the given `type`."
  [client type object-id new-value-map]
  (let [sf-type (get-sf-type-name client type)
        sf-value-map (sf-attrs client type new-value-map)]
    (sf/update! client sf-type object-id sf-value-map)))

(s/fdef create!
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :values (s/map-of ::sc/attr ::sc/value))
  :ret ::spec/id)

(defn ^:spark/no-boot-spec-coverage create!
  [client type new-value-map]
  (let [sf-type (get-sf-type-name client type)
        sf-value-map (sf-attrs client type new-value-map)]
    (sf/create! client sf-type sf-value-map)))

(s/fdef import!
  :args (s/cat :client ::sf/client
               :type ::sc/attr
               :records (s/coll-of (s/map-of ::sc/attr ::sc/value)))
  :ret (s/and (partial instance? clojure.lang.IDeref)
              (comp (partial s/valid? (s/coll-of any?)) deref))
  :fn (fn [{:keys [args ret]}]
        (= (count (:records args)) (count @ret))))

(defn ^:spark/no-boot-spec-coverage import!
  [client type records]
  (let [sf-type (get-sf-type-name client type)
        sf-value-maps (mapv (partial sf-attrs client type) records)]
    (sf/import! client sf-type sf-value-maps)))

(defn delete!
  [client type id]
  (let [sf-type (get-sf-type-name client type)]
    (sf/delete! client sf-type id)))
