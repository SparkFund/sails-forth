# sails-forth

Sails-forth is a clojure salesforce library. It is fully typed with core.typed.
It uses a stateful client object to transparently handle authentication and
version detection.

## Usage

``` clojure
(require '[sails-forth :as sf])

(def config
  {:username "..."
   :password "..."
   :token "..."
   :consumer-key "..."
   :consumer-secret "..."
   :sandbox? false})

(def client (sf/build-client config))

(sf/request! client :get "/limits" {})

(def object-id
  (sf/create! client "contact" {:first_name "Spark" :last_name "Fund"}))

(sf/delete! client "contact" object-id)
```

## Configuration

You may find http://www.calvinfroedge.com/salesforce-how-to-generate-api-credentials/
useful to help create your consumer-key and consumer-secret values.

## License

Copyright © 2015 SparkFund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
