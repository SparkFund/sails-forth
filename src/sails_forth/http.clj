(ns sails-forth.http
  (:require [cheshire.parse]
            [clj-http.client :as http]
            [clojure.spec :as s]
            [sails-forth.spec :as spec]))

(def http-methods
  #{:get :post :patch :put :delete})

(s/def ::method
  http-methods)

(s/def ::url
  string?)

(s/def ::param
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

(defn json-request
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

(s/def ::authentication
  (s/keys :req-un [::instance_url
                   ::access_token]))

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

(s/def read-only?
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
                   ::authentication
                   ::version-url
                   ::requests
                   ::read-only?
                   ::config]))

(s/fdef authenticate
  :args (s/cat :config ::config)
  :ret (s/nilable ::authentication))

(defn authenticate
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
               (s/valid? ::authentication body))
      body)))

(s/def ::version
  (s/keys :req-un [::url]))

(s/fdef versions
  :args (s/cat :url ::url)
  :ret (s/nilable (s/coll-of ::url :kind vector?)))

(defn versions
  [url]
  (let [url (str url "/services/data/")
        response (json-request :get {} url nil)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (s/valid? (s/coll-of ::version) body))
      (mapv #(get % :url) body))))

(s/def ::api-hosts
  #{"test.salesforce.com" "login.salesforce.com"})

(s/fdef derive-host
  :args (s/cat :config ::config)
  :ret (s/or :implied ::api-hosts
             :given ::host))

(defn derive-host
  [config]
  (let [{:keys [sandbox? host]} config]
    (or host (if sandbox? "test.salesforce.com" "login.salesforce.com"))))

(s/fdef build-state
  :args (s/cat :config ::config)
  :ret ::state)

(defn build-state
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

(defn try-authentication
  [state]
  (let [{:keys [authentication config requests]} state]
    (cond-> state
      (not authentication)
      (assoc :authentication (authenticate config)
             :requests (inc requests)))))

(s/fdef try-to-find-latest-version
  :args (s/cat :state ::state)
  :ret ::state)

(defn try-to-find-latest-version
  [state]
  (let [{:keys [authentication requests version-url]} state]
    (cond-> state
      (and (not version-url) authentication)
      (assoc :version-url (last (versions (:instance_url authentication)))
             :requests (inc requests)))))

(s/fdef request
  :args (s/cat :state ::state :method ::method :url ::url :params ::params)
  :ret (s/tuple ::state (s/nilable ::response)))

(defn request
  [state method url params]
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
          {:keys [authentication requests version-url]} state
          response (when-let [{:keys [access_token instance_url]} authentication]
                     (let [url (if (and version-url
                                        (.startsWith ^String url version-url))
                                 (str instance_url url)
                                 (str instance_url version-url url))
                           headers {"Authorization" (str "Bearer " access_token)}]
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
               :url ::url
               :params ::params)
  :ret (s/nilable ::response))

(defn request!
  "Issue the given request using the given client"
  [client method url params]
  (let [[client' response] (request @client method url params)]
    ;; TODO could try to merge states, otherwise request count may be incorrect
    (reset! client client')
    response))

(s/fdef build-client!
  :args (s/cat :config ::config)
  :ret ::client)

(defn build-client!
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

(defn create!
  "Creates an object of the given type and attrs using the given salesforce
   client. If salesforce responds successfully, this returns the object's id,
   otherwise this raises an exception."
  [client type attrs]
  (let [url (str "/sobjects/" type)
        response (request! client :post url attrs)
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

(defn delete!
  "Deletes the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise."
  [client type id]
  (let [url (str "/sobjects/" type "/" id)
        response (request! client :delete url {})
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

(defn update!
  "Updates the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise."
  [client type id attrs]
  (let [url (str "/sobjects/" type "/" id)
        response (request! client :patch url attrs)
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

(defn list!
  [client type]
  (let [url (str "/sobjects/" type)
        response (request! client :get url {})
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

(defn describe!
  [client type]
  (let [url (str "/sobjects/" type "/describe")
        response (request! client :get url {})
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

(defn objects!
  [client]
  (let [url "/sobjects"
        response (request! client :get url {})
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

(defn query!
  "Executes the given query and returns all results, eagerly fetching if there
   is pagination"
  [client query]
  (let [url "/query"
        params {:q query}
        response (request! client :get url params)]
    (loop [response response
           results []]
      (let [{:keys [status body]} response]
        (if (and (= 200 status)
                 (s/valid? ::spec/query-results body))
          (let [results (into results (get body :records))]
            (if (get body :done)
              results
              (let [url (get body :nextRecordsUrl)]
                (recur (request! client :get url {}) results))))
          (let [data {:query query
                      :status status
                      :body body}
                message "Could not execute salesforce query"]
            (throw (ex-info message data))))))))

(s/fdef count!
  :args (s/cat :client ::client
               :query ::spec/query)
  :ret nat-int?)

(defn count!
  "Executes the given query and returns the total number of results.
   This is intended for use with COUNT() queries."
  [client query]
  (let [url "/query"
        params {:q query}
        response (request! client :get url params)]
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

(defn limits!
  [client]
  (let [response (request! client :get "/limits" {})
        {:keys [status body]} response]
    (if (and (= 200 status)
             (s/valid? ::spec/limits body))
      body
      (let [data {:status status
                  :body body}
            message "Could not find salesforce limits"]
        (throw (ex-info message data))))))
