(ns sails-forth
  (:require [clj-http.client :as http]
            [clojure.core.typed :as t]))

(t/defalias HttpMethod
  (t/U ':get ':post ':patch ':put ':delete))

(t/defalias HttpUrl
  t/Str)

(t/defalias HttpParams
  (t/Map t/Kw t/Any))

(t/defalias HttpStatus
  t/Int)

(t/defalias Json
  (t/Rec [v]
         (t/U t/Str
              t/Num
              t/Bool
              (t/Value nil)
              (t/Vec v)
              (t/Map t/Str v))))

(t/defalias JsonMap
  (t/Map t/Str Json))

(t/defalias HttpBody
  Json)

(t/defalias HttpHeaders
  (t/Map t/Str t/Str))

(t/defalias HttpRequest
  (t/HMap :mandatory {:method HttpMethod
                      :url HttpUrl}))

(t/defalias HttpResponse
  (t/HMap :mandatory {:status HttpStatus}
          :optional {:body HttpBody
                     :headers HttpHeaders}))

(t/ann ^:no-check clj-http.client/request [HttpRequest -> HttpResponse])

(t/defn json-request
  [method :- HttpMethod
   headers :- HttpHeaders
   url :- HttpUrl
   params :- (t/Option HttpParams)] :- HttpResponse
  (let [request (cond-> {:method method
                         :url url
                         :throw-exceptions false
                         :accept :json
                         :coerce :always
                         :as :json-string-keys}
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
    (prn request)
    (http/request request)))

(t/defalias Authentication
  (t/HMap :mandatory {:instance-url t/Str
                      :access-token t/Str}
          :complete? true))

(t/defalias Config
  (t/HMap :mandatory {:username t/Str
                      :password t/Str
                      :token t/Str
                      :consumer-key t/Str
                      :consumer-secret t/Str}
          :optional {:version t/Int
                     :sandbox? t/Bool
                     :host t/Str}))

(t/defalias State
  (t/HMap :mandatory {:host t/Str
                      :authentication (t/Option Authentication)
                      :version-url (t/Option HttpUrl)
                      :requests t/Int
                      :config Config}
          :complete? true))

(t/defn authenticate
  [config :- Config] :- (t/Option Authentication)
  (let [{:keys [host username password token consumer-key consumer-secret]} config
        params {:username username
                :password (str password token)
                :client_id consumer-key
                :client_secret consumer-secret
                :grant_type "password"}
        url (str "https://" host "/services/oauth2/token")
        response (json-request :post {} url params)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (map? body))
      (let [{:strs [instance_url access_token]} body]
        (when (and (string? instance_url)
                   (string? access_token))
          {:instance-url instance_url
           :access-token access_token})))))

(t/ann ^:no-check versions [t/Str -> (t/Option (t/Seq HttpUrl))])
(defn versions
  [url]
  (let [url (str url "/services/data/")
        response (json-request :get {} url nil)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (sequential? body)
               (every? true? (map map? body)))
      (let [urls (map #(get % "url") body)]
        (when (every? identity urls)
          urls)))))

(t/defn derive-host
  [config :- Config] :- t/Str
  (let [{:keys [sandbox? host]} config]
    (or host (if sandbox? "test.salesforce.com" "login.salesforce.com"))))

(t/defn build-state
  [config :- Config] :- State
  (let [{:keys [version]} config
        host (derive-host config)]
    (cond-> {:authentication nil
             :version-url nil
             :requests 0
             :host host
             :config (assoc config :host host)}
      version (assoc :version-url (str "/services/data/v" version)))))

(t/defn try-authentication
  [state :- State] :- State
  (let [{:keys [authentication config requests]} state]
    (cond-> state
      (not authentication)
      (assoc :authentication (authenticate config)
             :requests (inc requests)))))

(t/defn try-to-find-latest-version
  [state :- State] :- State
  (let [{:keys [authentication requests version-url]} state]
    (cond-> state
      (and (not version-url) authentication)
      (assoc :version-url (last (versions (:instance-url authentication)))
             :requests (inc requests)))))

(t/defn request
  [state :- State
   method :- HttpMethod
   url :- HttpUrl
   params :- HttpParams] :- (t/HVec [State (t/Option HttpResponse)])
  (t/loop [state :- State state
           tries :- t/Int 0]
    (let [state (-> state
                    try-authentication
                    try-to-find-latest-version)
          {:keys [authentication requests version-url]} state
          response (when-let [{:keys [access-token instance-url]} authentication]
                     (let [url (str instance-url version-url url)
                           headers {"Authorization" (str "Bearer " access-token)}]
                       (json-request method headers url params)))
          {:keys [status body]} response
          state (cond-> state
                  authentication
                  (assoc :requests (inc requests)))]
      (if (and (= 401 status)
               (= tries 0))
        (recur (assoc state :authentication nil) (inc tries))
        [state response]))))

(t/defalias SalesforceClient
  (t/Atom1 State))

(t/defn request!
  "Issue the given request using the given client"
  [client :- SalesforceClient
   method :- HttpMethod
   url :- HttpUrl
   params :- HttpParams] :- (t/Option HttpResponse)
  (let [[client' response] (request @client method url params)]
    ;; TODO could try to merge states, otherwise request count may be incorrect
    (reset! client client')
    response))

(t/defalias SalesforceId
  t/Str)

(t/defalias SalesforceType
  t/Str)

(t/defalias SalesforceAttrs
  (t/Map t/Keyword t/Str))

(t/defalias SalesforceQuery
  t/Str)

(t/defn build-client!
  "Creates a stateful Salesforce client from the given config. The client
   authenticates lazily and uses the latest Salesforce version if none is
   specified. If an authenticated request receives an invalid authentication
   response, the client will try to reauthenticate and retry the request.

   The client may be used concurrently, but it may unnecessarily attempt
   to authenticate concurrently and may not update its internal request count
   correctly.

   This fn explicitly makes no guarantees regarding the type of the client
   entity, other than it can be used with the user-facing fns in this ns."
  [config :- Config] :- SalesforceClient
  (atom (build-state config)))

(t/defn create!
  "Creates an object of the given type and attrs using the given salesforce
   client. If salesforce responds successfully, this returns the object's id,
   otherwise this raises an exception."
  [client :- SalesforceClient
   type :- SalesforceType
   attrs :- SalesforceAttrs] :- SalesforceId
  (let [url (str "/sobjects/" type)
        response (request! client :post url attrs)
        {:keys [status body]} response]
    (if (and (= 201 status)
             (map? body))
      (let [{:strs [id]} body]
        (t/cast t/Str id))
      (let [data {:type type
                  :attrs attrs
                  :status status
                  :body body}
            message (case status
                      400 "Could not create invalid salesforce object"
                      nil "Could not authenticate to salesforce"
                      "Invalid salesforce response")]
        (throw (ex-info message data))))))

(t/defn delete!
  "Deletes the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise."
  [client :- SalesforceClient
   type :- SalesforceType
   id :- SalesforceId] :- (t/Value true)
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

(t/defn update!
  "Updates the object of the given type with the given id. This returns true
   if it succeeds and raises an exception otherwise."
  [client :- SalesforceClient
   type :- SalesforceType
   id :- SalesforceId
   attrs :- SalesforceAttrs] :- (t/Value true)
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

(t/defn list!
  [client :- SalesforceClient
   type :- SalesforceType] :- (t/Option JsonMap)
  (let [url (str "/sobjects/" type)
        response (request! client :get url {})
        {:keys [status body]} response]
    ;; TODO t/cast doesn't work with Json or JsonMap
    (cond (and (= 200 status)
               ((t/pred JsonMap) body))
          body
          (= 404 status)
          nil
          :else
          (let [data {:type type
                      :status status
                      :body body}
                message "Could not retrieve list of salesforce objects"]
            (throw (ex-info message data))))))

(t/defn describe!
  [client :- SalesforceClient
   type :- SalesforceType] :- (t/Option JsonMap)
  (let [url (str "/sobjects/" type "/describe")
        response (request! client :get url {})
        {:keys [status body]} response]
    (cond (and (= 200 status)
               ((t/pred JsonMap) body))
          body
          (= 404 status)
          nil
          :else
          (let [data {:type type
                      :status status
                      :body body}
                message "Could not retrieve list of salesforce objects"]
            (throw (ex-info message data))))))

;; TODO this should be able to return JsonMap alone?
;; TODO this has some pagination feature; should we transparently concat them?
(t/defn query!
  [client :- SalesforceClient
   query :- SalesforceQuery] :- (t/Option JsonMap)
  (let [url "/query"
        params {:q query}
        response (request! client :get url params)
        {:keys [status body]} response]
    (if (and (= 200 status)
             ((t/pred JsonMap) body))
      body
      (let [data {:query query
                  :status status
                  :body body}
            message "Could not execute salesforce query"]
        (throw (ex-info message data))))))
