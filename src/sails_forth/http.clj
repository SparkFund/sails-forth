(ns sails-forth.http
  (:require [cheshire.parse]
            [clj-http.client :as http]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [sails-forth.spec :as spec]))

(def http-methods
  #{:get :post :patch :put :delete})

(s/def ::method
  http-methods)

(s/def ::url
  string?)

(s/def ::params
  (s/map-of keyword? any?))

(s/def ::status
  (s/int-in 100 600))

(s/def ::body
  ::spec/json-map)

(s/def ::headers
  (s/map-of string? string?))

(s/def ::request
  (s/keys :req-un [::method ::url]))

(s/def ::response
  (s/keys ::req-un [::status]
          ::opt-un [::body ::headers]))

(s/fdef json-request
  :args (s/cat :method ::method
               :headers ::headers
               :url ::url
               :params (s/nilable ::params))
  :ret ::response)

(defn ^:spark/no-boot-spec-coverage json-request
  [method headers url params]
  (let [request (cond-> {:method method
                         :url url
                         :throw-exceptions false
                         :accept :json
                         :coerce :always
                         :as :json}
                  (seq headers)
                  (assoc :headers headers)
                  (and (not (nil? params))
                       (or (= :post method)
                           (= :patch method)))
                  (assoc :form-params params
                         :content-type :json)
                  (and (seq params)
                       (= :get method))
                  (assoc :query-params params))]
    (binding [cheshire.parse/*use-bigdecimals?* true]
      (http/request request))))

(s/def ::instance_url
  ::url)

(s/def ::access_token
  string?)

(s/def ::non-nil-authentication
  (s/keys :req-un [::instance_url
                   ::access_token]))

(s/def ::authentication
  (s/nilable ::non-nil-authentication))

(s/def ::username
  string?)

(s/def ::password
  string?)

(s/def ::token
  string?)

(s/def ::consumer-key
  string?)

(s/def ::consumer-secret
  string?)

(s/def ::version
  string?)

(s/def ::sandbox?
  boolean?)

(s/def ::host
  string?)

(s/def ::read-only?
  boolean?)

(s/def ::config
  (s/keys :req-un [::username
                   ::password
                   ::token
                   ::consumer-key
                   ::consumer-secret]
          :opt-un [::version
                   ::sandbox?
                   ::host
                   ::read-only?]))

(s/def ::version-url
  (s/nilable ::url))

(s/def ::requests
  (s/or :zero zero?
        :pos pos-int?))

(s/def ::state
  (s/keys :req-un [::host
                   ::requests
                   ::read-only?
                   ::config]
          :opt-un [::authentication
                   ::version-url
                   ::version]))

(s/fdef authenticate
  :args (s/cat :config ::config)
  :ret ::authentication)

(defn ^:spark/no-boot-spec-coverage authenticate
  [config]
  (let [{:keys [host username password token consumer-key consumer-secret]} config
        params {:username username
                :password (str password token)
                :client_id consumer-key
                :client_secret consumer-secret
                :grant_type "password"}
        url (str "https://" host "/services/oauth2/token")
        ;; TODO this is just like json-request except the body is form encoded
        request {:method :post
                 :url url
                 :throw-exceptions false
                 :form-params params
                 :accept :json
                 :coerce :always
                 :as :json}
        response (http/request request)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (s/valid? ::non-nil-authentication body))
      body)))

(s/def ::version-map
  (s/keys :req-un [::url
                   ::version]))

(s/def ::versions
  (s/coll-of ::version-map))

(s/fdef versions
  :args (s/cat :url ::url)
  :ret (s/nilable ::versions))

(defn ^:spark/no-boot-spec-coverage versions
  [url]
  (let [url (str url "/services/data/")
        response (json-request :get {} url nil)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (s/valid? ::versions body))
      body)))

(s/def ::api-hosts
  #{"test.salesforce.com" "login.salesforce.com"})

(s/fdef derive-host
  :args (s/cat :config ::config)
  :ret (s/or :implied ::api-hosts
             :given ::host))

(defn ^:spark/no-boot-spec-coverage derive-host
  [config]
  (let [{:keys [sandbox? host]} config]
    (or host (if sandbox? "test.salesforce.com" "login.salesforce.com"))))

(s/fdef build-state
  :args (s/cat :config ::config)
  :ret ::state)

(defn ^:spark/no-boot-spec-coverage build-state
  [config]
  (let [{:keys [version]} config
        host (derive-host config)
        read-only? (get config :read-only? false)]
    (cond-> {:authentication nil
             :version-url nil
             :requests 0
             :host host
             :read-only? read-only?
             :config (assoc config :host host)}
      version (assoc :version-url (str "/services/data/v" version)))))

(s/fdef try-authentication
  :args (s/cat :state ::state)
  :ret ::state)

(defn ^:spark/no-boot-spec-coverage try-authentication
  [state]
  (let [{:keys [authentication config requests]} state]
    (cond-> state
      (not authentication)
      (assoc :authentication (authenticate config)
             :requests (inc requests)))))

(s/fdef try-to-find-latest-version
  :args (s/cat :state ::state)
  :ret ::state)

(defn ^:spark/no-boot-spec-coverage try-to-find-latest-version
  [state]
  (let [{:keys [authentication requests version version-url]} state
        last-version (when (and (not (and version version-url))
                                authentication)
                       (last (versions (:instance_url authentication))))]
    (cond-> state
      last-version
      (assoc :version-url (:url last-version)
             :version (:version last-version)
             :requests (inc requests)))))

(s/def ::service
  #{:data :async})

(s/fdef request
  :args (s/cat :state ::state
               :method ::method
               :service ::service
               :url ::url
               :params ::params)
  :ret (s/tuple ::state (s/nilable ::response)))

(defn ^:spark/no-boot-spec-coverage request
  [state method service url params]
  (when (and (:read-only? state)
             (case method
               :post true
               :put true
               :patch true
               :delete true
               false))
    (let [data {:method method
                :url url
                :params params}
          message "Read-only clients may not issue requests with side effects"]
      (throw (ex-info message data))))
  (loop [state state
         tries 0]
    (let [state (-> state
                    try-authentication
                    try-to-find-latest-version)
          {:keys [authentication requests version version-url]} state
          response (when-let [{:keys [access_token instance_url]} authentication]
                     (let [[url-pattern headers]
                           ;; Salesforce: not a shining example of consistency
                           (case service
                             :data ["/services/%s/v%s"
                                    {"Authorization" (str "Bearer " access_token)}]
                             :async ["/services/%s/%s"
                                     {"X-SFDC-Session" access_token}])
                           prefix (format url-pattern (name service) version)
                           url (if (string/starts-with? url prefix)
                                 (str instance_url url) ;; url already knows its service and version, dont append prefix a second time
                                 (str instance_url prefix url))]
                       (json-request method headers url params)))
          {:keys [status body]} response
          state (cond-> state
                  authentication
                  (assoc :requests (inc requests)))]
      (if (and (= 401 status)
               (= tries 0))
        (recur (assoc state :authentication nil) (inc tries))
        [state response]))))

(s/def ::client
  (s/and (partial instance? clojure.lang.Atom)
         (comp (partial s/valid? ::state) deref)))

(s/fdef request!
  :args (s/cat :client ::client
               :method ::method
               :service ::service
               :url ::url
               :params ::params)
  :ret (s/nilable ::response))

(defn ^:spark/no-boot-spec-coverage request!
  "Issue the given request using the given client"
  [client method service url params]
  (let [[client' response] (request @client method service url params)]
    ;; TODO could try to merge states, otherwise request count may be incorrect
    (reset! client client')
    response))

(s/fdef build-client!
  :args (s/cat :config ::config)
  :ret ::client)

(defn ^:spark/no-boot-spec-coverage build-client!
  "Creates a stateful Salesforce client from the given config. The client
   authenticates lazily and uses the latest Salesforce version if none is
   specified. If an authenticated request receives an invalid authentication
   response, the client will try to reauthenticate and retry the request.

   The client may be used concurrently, but it may unnecessarily attempt
   to authenticate concurrently and may not update its internal request count
   correctly.

   This fn explicitly makes no guarantees regarding the type of the client
   entity, other than it can be used with the user-facing fns in this ns."
  [config]
  (atom (build-state config)))

(s/fdef create!
  :args (s/cat :client ::client
               :type ::spec/type
               :attrs ::spec/attrs)
  :ret ::spec/id)

(defn ^:spark/no-boot-spec-coverage create!
  "Creates an object of the given type and attrs using the given salesforce
   client. If salesforce responds successfully, this returns the object's id,
   otherwise this raises an exception."
  [client type attrs]
  (let [url (str "/sobjects/" type)
        response (request! client :post :data url attrs)
        {:keys [status body]} response]
    (if (and (= 201 status)
             (s/valid? ::spec/entity body))
      (get body :id)
      (let [data {:type type
                  :attrs attrs
                  :status status
                  :body body}
            message (case status
                      400 "Could not create invalid salesforce object"
                      nil "Could not authenticate to salesforce"
                      "Invalid salesforce response")]
        (throw (ex-info message data))))))

(s/fdef delete!
  :args (s/cat :client ::client
               :type ::spec/type
               :id ::spec/id)
  :ret boolean?)

(defn ^:spark/no-boot-spec-coverage delete!
  "Deletes the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise."
  [client type id]
  (let [url (str "/sobjects/" type "/" id)
        response (request! client :delete :data url {})
        {:keys [status body]} response]
    (if (= 204 status)
      true
      (let [data {:type type
                  :id id
                  :status status
                  :body body}
            message (case status
                      nil "Could not authenticate to salesforce"
                      "Invalid salesforce response")]
        (throw (ex-info message data))))))

(s/fdef update!
  :args (s/cat :client ::client
               :type ::spec/type
               :id ::spec/id
               :attrs ::spec/attrs)
  :ret boolean?)

(defn ^:spark/no-boot-spec-coverage update!
  "Updates the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise."
  [client type id attrs]
  (let [url (str "/sobjects/" type "/" id)
        response (request! client :patch :data url attrs)
        {:keys [status body]} response]
    (if (= 204 status)
      true
      (let [data {:type type
                  :id id
                  :status status
                  :body body}
            message (case status
                      nil "Could not authenticate to salesforce"
                      "Invalid salesforce response")]
        (throw (ex-info message data))))))

(s/fdef list!
  :args (s/cat :client ::spec/client
               :type ::spec/type)
  :ret (s/nilable ::spec/json-map))

(defn ^:spark/no-boot-spec-coverage list!
  [client type]
  (let [url (str "/sobjects/" type)
        response (request! client :get :data url {})
        {:keys [status body]} response]
    (cond (and (= 200 status)
               (s/valid? ::spec/json-map body))
          body
          (= 404 status)
          nil
          :else
          (let [data {:type type
                      :status status
                      :body body}
                message "Could not retrieve list of salesforce objects"]
            (throw (ex-info message data))))))

(s/fdef describe!
  :args (s/cat :client ::client
               :type ::spec/type)
  :ret (s/nilable ::spec/object-description))

(defn ^:spark/no-boot-spec-coverage describe!
  [client type]
  (let [url (str "/sobjects/" type "/describe")
        response (request! client :get :data url {})
        {:keys [status body]} response]
    (cond (and (= 200 status)
               (s/valid? ::spec/object-description body))
          body
          (= 404 status)
          nil
          :else
          (let [data {:type type
                      :status status
                      :body body}
                message "Could not retrieve description of salesforce object"]
            (throw (ex-info message data))))))

(s/fdef objects!
  :args (s/cat :client ::client)
  :ret ::spec/objects-overview)

(defn ^:spark/no-boot-spec-coverage objects!
  [client]
  (let [url "/sobjects"
        response (request! client :get :data url {})
        {:keys [status body]} response]
    (cond (and (= 200 status)
               (s/valid? ::spec/objects-overview body))
          body
          :else
          (let [data {:status status
                      :body body}
                message "Could not retrieve list of salesforce objects"]
            (throw (ex-info message data))))))

(s/fdef query!
  :args (s/cat :client ::client
               :query ::spec/query)
  :ret ::spec/records)

(defn ^:spark/no-boot-spec-coverage query!
  "Executes the given query and returns all results, eagerly fetching if there
   is pagination"
  [client query]
  (let [url "/query"
        params {:q query}
        response (request! client :get :data url params)]
    (loop [response response
           results []]
      (let [{:keys [status body]} response]
        (if (and (= 200 status)
                 (s/valid? ::spec/query-results body))
          (let [results (into results (get body :records))]
            (if (get body :done)
              results
              (let [url (get body :nextRecordsUrl)]
                (recur (request! client :get :data url {}) results))))
          (let [data {:query query
                      :status status
                      :body body}
                message "Could not execute salesforce query"]
            (throw (ex-info message data))))))))

(s/fdef count!
  :args (s/cat :client ::client
               :query ::spec/query)
  :ret nat-int?)

(defn ^:spark/no-boot-spec-coverage count!
  "Executes the given query and returns the total number of results.
   This is intended for use with COUNT() queries."
  [client query]
  (let [url "/query"
        params {:q query}
        response (request! client :get :data url params)]
    (let [{:keys [status body]} response]
      (if (and (= 200 status)
               (s/valid? ::spec/count-query-results body))
        (get body :totalSize)
        (let [data {:query query
                    :status status
                    :body body}
              message "Could not execute salesforce count query"]
          (throw (ex-info message data)))))))

(s/fdef limits!
  :args (s/cat :client ::client)
  :ret ::spec/limits)

(defn ^:spark/no-boot-spec-coverage limits!
  [client]
  (let [response (request! client :get :data "/limits" {})
        {:keys [status body]} response]
    (if (and (= 200 status)
             (s/valid? ::spec/limits body))
      body
      (let [data {:status status
                  :body body}
            message "Could not find salesforce limits"]
        (throw (ex-info message data))))))

(s/def ::job-operation
  #{:insert})

(s/def :sails-forth.http.job/state
  #{"Open" "Closed"})

(s/def ::job
  (s/keys :req-un [::spec/id
                   :sails-forth.http.job/state]))

(s/fdef create-import-job!
  :args (s/cat :client ::client
               :type ::spec/type
               :operation ::job-operation)
  :ret (s/nilable ::spec/id))

(defn ^:spark/no-boot-spec-coverage create-import-job!
  [client type operation]
  (let [params {:operation (name operation)
                :object type
                :contentType "JSON"}
        response (request! client :post :async "/job" params)
        {:keys [status body]} response]
    (when (and (= 201 status)
               (s/valid? ::job body)
               (= "Open" (:state body)))
      (:id body))))

(s/fdef close-import-job!
  :args (s/cat :client ::client
               :id ::spec/id)
  :ret (s/nilable true?))

(defn ^:spark/no-boot-spec-coverage close-import-job!
  [client id]
  (let [params {:state "Closed"}
        response (request! client :post :async (str "/job/" id) params)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (s/valid? ::job body)
               (= "Closed" (:state body)))
      true)))

(s/def ::batch
  (s/keys :req-un [::spec/id]))

(s/fdef add-import-batch!
  :args (s/cat :client ::client
               :id ::spec/id
               :records (s/coll-of ::spec/attrs))
  :ret ::spec/id)

(defn ^:spark/no-boot-spec-coverage add-import-batch!
  [client id records]
  (let [url (str "/job/" id "/batch")
        response (request! client :post :async url records)
        {:keys [status body]} response]
    (when (and (= 201 status)
               (s/valid? ::batch body))
      (:id body))))

(s/def ::success
  boolean?)

(s/def ::created
  boolean?)

(s/def ::batch-results
  (s/coll-of (s/keys :req-un [::success])))

(defn get-batch-results!
  [client job-id batch-id]
  (let [url (str "/job/" job-id "/batch/" batch-id "/result")
        response (request! client :get :async url {})
        {:keys [status body]} response]
    (when (and (= 200 status)
               (s/valid? ::batch-results body))
      body)))

(def import-poll-timeout-ms
  (* 60 1000))

(def import-total-tries
  10)

(defn import!
  [client type records]
  (let [job-id (create-import-job! client type :insert)
        batch-id (when job-id
                   (add-import-batch! client job-id records))]
    (when (and batch-id (close-import-job! client job-id))
      (delay (loop [tries 0]
               (or (get-batch-results! client job-id batch-id)
                   (when (< tries import-total-tries)
                     (Thread/sleep import-poll-timeout-ms)
                     (recur (inc tries)))))))))

(defn list-actions!
  "Lists actions that can be performed at the given path."
  [client path]
  (let [url (str "/actions/" path)
        response (request! client :get :data url nil)
        {:keys [status body]} response]
    (when (= status 200)
      (if (contains? body :actions)
        (vec (for [action (:actions body)]
               (str path "/" (:name action))))
        (mapv #(str path "/" (name %)) (keys body))))))

(defn describe-action!
  "Describes the action at the given path."
  [client action]
  (let [url (str "/actions/" action)
        response (request! client :get :data url nil)
        {:keys [status body]} response]
    (when (= status 200)
      (when (contains? body :inputs)
        body))))

(defn take-action!
  [client action inputs]
  (let [url (str "/actions/" action)
        response (request! client :post :data url {"inputs" inputs})
        {:keys [status body]} response]
    (if (and (= status 200)
             (every? :isSuccess body))
      (mapv (comp :output :outputValues) body)
      (throw (ex-info "action failed" response)))))
