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
              (t/Value nil)
              (t/Seq v)
              (t/Map t/Str v))))

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
                         :headers headers
                         :url url
                         :throw-exceptions false
                         :accept :json
                         :coerce :always
                         :as :json-string-keys}
                  (and (seq params)
                       (= :post method))
                  (assoc :form-params params))]
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
               (every? (map map? body)))
      (let [urls (map #(get % "url") body)]
        (when (every? identity urls)
         urls)))))

(t/defn derive-host
  [config :- Config] :- t/Str
  (let [{:keys [sandbox? host]} config]
    (or host (if sandbox? "test.salesforce.com" "login.salesforce.com"))))

(t/defn build-state
  [config :- Config] :- State
  (let [{:keys [version]} config]
    (cond-> {:authentication nil
             :version-url nil
             :requests 0
             :host (derive-host config)
             :config config}
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
          {:keys [authentication requests]} state
          response (when-let [{:keys [access-token]} authentication]
                     (let [headers {"Authorization" (str "Bearer " access-token)}]
                       (json-request method headers url params)))
          {:keys [status body]} response
          state (cond-> state
                  authentication
                  (assoc :requests (inc requests)))]
      (if (and (= 401 status)
               (= tries 0))
        (recur (assoc state :authentication nil) (inc tries))
        [state response]))))

(t/defn request!
  [state :- (t/Atom1 State)
   method :- HttpMethod
   url :- HttpUrl
   params :- HttpParams] :- (t/Option HttpResponse)
  (let [[state' response] (request @state method url params)]
    ;; TODO could try to merge states, otherwise request count may be incorrect
    (reset! state state')
    response))
