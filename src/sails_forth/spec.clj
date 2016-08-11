(ns sails-forth.spec
  (:require [clojure.spec :as s]))

(s/def ::id
  string?)

(s/def ::type
  string?)

(s/def ::field
  keyword?)

(s/def ::attrs
  (s/map-of ::field ::json-simple))

(s/def ::query
  string?)

(s/def ::entity
  (s/keys :req-un [::id]))

(s/def ::json-simple
  (s/or :string string?
        :number bigdec?
        :nil nil?
        :boolean boolean?))

(s/def ::json
  (s/or :simple ::json-simple
        :vector (s/coll-of ::json :kind vector?)
        :map (s/map-of keyword? ::json)))

(s/def ::json-map
  (s/map-of keyword? ::json))

(s/def ::referenceTo
  (s/coll-of ::type :kind vector?))

(s/def ::scale
  nat-int?)

(s/def ::precision
  nat-int?)

(s/def ::label
  string?)

(s/def ::relationshipName
  any?)

(s/def ::field-description
  (s/keys :req-un [::name
                   ::type
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))

(s/def ::custom
  boolean?)

(s/def ::fields
  (s/coll-of ::field-description :kind vector?))

(s/def ::object-description
  (s/keys ::req-un [::name
                    ::label
                    ::custom
                    ::fields]))

(s/def ::object-overview
  (s/keys :req-un [::name
                   ::label
                   ::custom]))

(s/def ::sobjects
  (s/coll-of ::object-overview :kind vector?))

(s/def ::objects-overview
  (s/keys :req-un [::sobjects]))

(s/def ::done
  boolean?)

(s/def ::totalSize
  nat-int?)

(s/def ::records
  (s/coll-of ::json-map :kind vector?))

(s/def ::nextRecordsUrl
  string?)

(s/def ::query-results
  (s/and (s/keys :req-un [::done
                          ::totalSize
                          ::records]
                 :opt-un [::nextRecordsUrl])
         #(if (:done %)
            (not (:nextRecordsUrl %))
            (:nextRecordsUrl %))))

(s/def ::count-query-results
  (s/and (s/keys :req-un [::done
                          ::totalSize
                          ::records])
         #(:done %)
         #(every? empty (:records %))))

(s/def ::Max
  nat-int?)

(s/def ::Remaining
  nat-int?)

(s/def ::limit
  (s/keys :req-un [::Max
                   ::Remaining]))

(s/def ::limits
  (s/map-of keyword? ::limit))
