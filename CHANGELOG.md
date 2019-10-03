## 0.11.2 (2019-10-02)

* nil denotes "null" in queries

## 0.10.1 (2019-07-08)

* Add support for comparison operators in soql in the memory client

## 0.10.0 (2019-04-08)

* Update to Clojure 10

## 0.9.1 (2018-09-06)

* Fix string escaping when building queries in `sails-forth.query`

## 0.9.0 (2018-07-23)

* Add `take-action!` implementation to memory client 
* Update spec for `build-memory-client`
* See [commits](https://github.com/SparkFund/sails-forth/compare/0.8.1...0.9.0)

## 0.8.1

* Introduce sails-forth.datomic ns with assert-query fn

## 0.8.0

* Update to clojure-1.9.0, also drop boot-mvn

## 0.7.0

* Update to clojure-1.9.0alpha17 and clojure.spec.alpha

## 0.6.0

* Fix incorrect specs
* Add spec-coverage
* Add actions-related methods

## 0.5.0

* Replace core.typed with clojure.spec
* Add import! fn, using the bulk api

## 0.4.1

* Add record-type-id fn and record-types cache

## 0.4.0

* Convert percent type values to decimal values
* Add render-value fn
* Use render-value when pushing updates using sails-forth.update

## 0.3.3

* Allow memory client to accept nil values
* Fix ns references

## 0.3.2

* Relax parentheses in generated soql to faciliate memory client use

## 0.3.1

* Revise memory schema to more closely resemble real schema

## 0.3.0

* Refactor client behind a protocol
* Add memory client to facilitate testing
* Deprecate top-level ns to make emacs clojure-mode happy

## 0.2.1

* Add update! fn

## 0.2.0

* Relax salesforce types

## 0.1.7

* Use ex-info for exceptions to provide better data

## 0.1.6

* Fix bug in resolving field paths for custom relations
* Add :where clause support to query

## 0.1.5

* Fix bug in field description type
* Add url metadata to sails-forth.query/query

## 0.1.4

* Add type registry to allow custom objects to have nicer type names
* Add helper fn to resolve attrs by labels

## 0.1.3

* Convert date fields to joda local-date instances
* Convert some double and int fields to integral types

## 0.1.2

* Add count! fn
* Add sails-forth.query ns
* Configure cheshire to parse json numbers as bigdecimals
* Parse dates and datetimes as joda instances

## 0.1.1

* Move user to sails-forth.repl and remove dependency on test ns

## 0.1.0

* Initial release: uses core.typed predicates to validate responses
