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
   url :- HttpUrl
   params :- (t/Option HttpParams)] :- HttpResponse
  (let [request (cond-> {:method method
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

(t/defn authenticate
  [host :- t/Str
   username :- t/Str
   password :- t/Str
   token :- t/Str
   consumer-key :- t/Str
   consumer-secret :- t/Str] :- (t/Option Authentication)
  (let [params {:username username
                :password (str password token)
                :client_id consumer-key
                :client_secret consumer-secret
                :grant_type "password"}
        url (str "https://" host "/services/oauth2/token")
        response (json-request :post url params)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (map? body))
      (let [{:strs [instance_url access_token]} body]
        (when (and instance_url access_token)
          ;; TODO why are the str calls necessary to type check?
          {:instance-url (str instance_url)
           :access-token (str access_token)})))))

(t/defalias VersionUrl HttpUrl)

(t/defn ^:no-check versions
  [url :- t/Str] :- (t/Option (t/Seq VersionUrl))
  (let [url (str url "/services/data/")
        response (json-request :get url nil)
        {:keys [status body]} response]
    (when (and (= 200 status)
               (sequential? body)
               (every? (map map? body)))
      (let [urls (map #(get % "url") body)]
        (when (every? identity urls)
          urls)))))

(t/defalias State
  (t/HMap :mandatory {:host t/Str
                      :authentication (t/Option Authentication)
                      :version-path (t/Option HttpUrl)
                      :requests t/Int}
          :complete? true))

(t/defalias Config
  (t/HMap :optional {:version t/Int
                     :sandbox? t/Bool
                     :host t/Str}))

(t/defn derive-host
  [config :- Config] :- t/Str
  (let [{:keys [sandbox? host]} config]
    (or host (if sandbox? "test.salesforce.com" "login.salesforce.com"))))

(t/defn build-state
  [config :- Config] :- State
  (let [{:keys [version]} config]
    (cond-> {:authentication nil
             :version-path nil
             :requests 0
             :host (derive-host config)}
      version (assoc :version-path (str "/services/data/v" version)))))

(t/defn request
  [state :- State
   method :- HttpMethod
   url :- HttpUrl
   params :- HttpParams] :- (t/HVec [State HttpResponse])
  [state {:status 200}])
