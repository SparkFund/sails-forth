# sails-forth

Sails-forth is a clojure salesforce library. It is fully typed with core.typed.
It uses a stateful client object to transparently handle authentication and
version detection.

## Installation

`sparkfund/sails-forth 0.1.5`

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

(sf/query! client "select First_Name, Last_Name from contact__c")
```

A higher-level query ns leverages the salesforce schema to denote types
and fields using more idiomatically clojure keywords. It also uses the
field types to coerce values. Numbers will always be interpreted from the
json payload as bigdecimals. Fixed point numbers with zero precision will
become longs if possible, bigintegers otherwise. Ints will become longs.
Dates and datetimes will become joda localdates and dates, respectively.

```clojure
(require '[sails-forth.query as sq])

(sq/query client {:find [:contact :id :first-name :last-name]})
```

## Configuration

You may find http://www.calvinfroedge.com/salesforce-how-to-generate-api-credentials/
useful to help create your consumer-key and consumer-secret values.

## License

Copyright © 2015-2016 SparkFund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
