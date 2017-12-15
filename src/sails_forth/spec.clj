(ns sails-forth.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::id
  string?)

(s/def ::type
  string?)

(s/def ::field
  (s/or :keyword keyword?
        :string string?))

(s/def ::attrs
  (s/map-of ::field ::json-simple))

(s/def ::query
  string?)

(s/def ::entity
  (s/keys :req-un [::id]))

(s/def ::json-simple
  (s/or :string string?
        ;; some internal unit tests fail with bigdec?
        :number number? ;bigdec?
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

(s/def ::name
  string?)

(s/def ::relationshipName
  any?)

(s/def ::value
  string?)

(s/def ::active
  boolean?)

;; other possible keys:
;;  :validFor
;;  :defaultValue
(s/def ::picklistValue
  (s/keys :req-un [
                   ; label can be nil in a picklistValue, is that correct? Awkward to express.
                   ; ::label

                   ::value
                   ::active]))

(s/def ::picklistValues
  (s/coll-of ::picklistValue
             :kind vector?))

(defmulti field-description-type :type)
(defmethod field-description-type "datetime" [_] (s/keys :req-un [::type]))
(defmethod field-description-type "date" [_] (s/keys :req-un [::type]))
(defmethod field-description-type "int" [_] (s/keys :req-un [::type]))
(defmethod field-description-type "percent" [_] (s/keys :req-un [::type ::scale ::precision]))
(defmethod field-description-type "double" [_] (s/keys :req-un [::type ::scale ::precision]))
(defmethod field-description-type "currency" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "id" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "string" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "reference" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "boolean" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "textarea" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "picklist" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "url" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "multipicklist" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "address" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "phone" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "email" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))
(defmethod field-description-type "encryptedstring" [_]
  (s/keys :req-un [::type
                   ::picklistValues
                   ::name
                   ::referenceTo
                   ::scale
                   ::precision
                   ::label
                   ::relationshipName]))

(s/def ::field-description
  (s/multi-spec field-description-type :type))

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
