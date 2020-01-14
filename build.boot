(def version "0.11.3")

(task-options!
  pom {:project 'sparkfund/sails-forth
       :version version
       :description "A Salesforce library"})

(set-env!
 :resource-paths #{"src"}
 :source-paths #{"test"}
 :dependencies
 '[[adzerk/bootlaces "0.1.13" :scope "test"]
   [adzerk/boot-jar2bin "1.1.0" :scope "test"]
   [adzerk/boot-test "1.2.0" :scope "test"]
   [cheshire "5.5.0"]
   [clj-http "2.0.0"]
   [clj-time "0.11.0"]
   [com.datomic/datomic-free "0.9.5561.62" :scope "test"]
   [org.clojure/clojure "1.10.0" :scope "provided"]
   [org.clojure/test.check "0.9.0" :scope "test"]
   [org.mule.tools/salesforce-soql-parser "2.0"]
   [sparkfund/boot-spec-coverage "0.4.0" :scope "test"]]
 :repositories
 #(conj %
        ["sparkfund" {:url "s3p://sparkfund-maven/releases/"}]
        ["mulesoft" {:url "https://repository.mulesoft.org/releases/"}])
 :wagons '[[sparkfund/aws-cli-wagon "1.0.4"]])

(require '[adzerk.boot-jar2bin :refer :all]
         '[adzerk.boot-test :as bt]
         '[clojure.java.io :as io]
         '[sparkfund.boot-spec-coverage :as cover])

(deftask deps
  [])

(def only-integration '(-> % meta :integration))
(def no-integration '(-> % meta :integration not))

(deftask test-all
  "Run every unit test, including integration tests"
  []
  (bt/test))

(deftask test-integration
  "Only run integration tests"
  []
  (bt/test
    :filters [only-integration]))

(deftask test
  "Run every non-integration test."
  []
  (bt/test
    :filters [no-integration]))

(deftask spec-coverage
  "Spec coverage checking using non-integration tests."
  []
  (cover/spec-coverage
    :filters [no-integration]
    :instrument 'sparkfund.boot-spec-coverage.instrument/in-n-outstrument))

(deftask spec-coverage-all
  "Spec coverage checking, including integration tests."
  []
  (cover/spec-coverage
    :instrument 'sparkfund.boot-spec-coverage.instrument/in-n-outstrument))

(require '[adzerk.bootlaces :refer :all])

(bootlaces! version)

(deftask release
  []
  (comp (build-jar) (push-release)))
