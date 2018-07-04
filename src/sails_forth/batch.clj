(ns sails-forth.batch
  (:require [clojure.spec.alpha :as s]
            [sails-forth.http :as http]
            [sails-forth.spec :as spec]))

(s/def ::batch-response (s/coll-of map?))

(s/fdef batch!
        :args (s/cat :client ::http/client
                     :type ::spec/type
                     :attrs ::spec/attrs)
        :ret ::batch-response)

(defn batch!
  [client type reqs]
  (let [url (str "/composite/" type)
        attrs {:batchRequests reqs}
        response (http/request! client :post :data url attrs)
        {:keys [status body]} response]
    (if (= 200 status)
      (get body :results)
      (let [data {:type type
                  :attrs attrs
                  :status status
                  :body body}
            message (case status
                      400 "Could not make batch request to salesforce"
                      nil "Could not authenticate to salesforce"
                      "Invalid salesforce response")]
        (throw (ex-info message data))))))
