# sails-forth

Sails-forth is a clojure salesforce library. It is specified with clojure.spec.
It uses a stateful client object to transparently handle authentication and
version detection.

## Installation

`[sparkfund/sails-forth "0.11.2"]`

Note that the memory client now uses a dependency that does not exist in
the common maven repositories. That repository is:

`https://repository.mulesoft.org/releases/`

If that presents a significant impediment, open an issue; we could look to
include it in our jar or seek permission to publish it on a standard maven.

## Usage

``` clojure
(require '[sails-forth.client :as sf])

(def config
  {:username "..."
   :password "..."
   :token "..."
   :consumer-key "..."
   :consumer-secret "..."
   :sandbox? false})

(def client (sf/build-http-client config))

(sf/limits! client {})

(def object-id
  (sf/create! client "contact" {:first_name "Spark" :last_name "Fund"}))

(sf/update! client "contact object-id {:last_name "Fondue"})

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
(sq/query client {:find [:contact :id :first-name :last-name [:faction :id :name]]})
(sq/query client {:find [:contact :id :first-name :last-name]
                  :where [[:= [:contract :last-name] "Organa"]]})
```

There is a memory client that operates on a schema and provides working impls of
all of the client fns. The query fn is limited to a subset of the soql operators,
including =, AND, OR, and IN, though adding support for more operators is very
straightforward.

``` clojure
(def mc (sf/build-memory-client (sf/schema client #{:contact})))
```

There is support for building transactions which can be applied to datomic
to record observations from salesforce queries:

``` clojure
(require '[sails-forth.datomic :as sd])

(def txns
  (sd/assert-query client "sf" {:find [:opportunity :id :name [:customer :id :name]]}))
```

## Configuration

You may find http://www.calvinfroedge.com/salesforce-how-to-generate-api-credentials/
useful to help create your consumer-key and consumer-secret values.

## License

Copyright © 2015-2019 Sparkfund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
