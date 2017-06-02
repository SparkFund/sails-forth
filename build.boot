(task-options!
  pom {:project     'sparkfund/sails-forth
       :version     "0.6.0-alpha2"
       :description "A Salesforce library"})

(set-env!
 :resource-paths #{"src"}
 :source-paths #{"test"}
 :dependencies
 '[[adzerk/boot-jar2bin "1.1.0" :scope "build"]
   [adzerk/boot-test "1.2.0" :scope "test"]
   [big-solutions/boot-mvn "0.1.5"]
   [sparkfund/spec-coverage "0.1.0" :scope "test"]
   [org.clojure/test.check "0.9.0" :scope "test"]
   [cheshire "5.5.0"]
   [clj-http "2.0.0"]
   [clj-time "0.11.0"]
   [com.github.jsqlparser/jsqlparser "0.9.5"]
   [org.clojure/clojure "1.9.0-alpha10"]])

(require '[adzerk.boot-jar2bin :refer :all]
         '[adzerk.boot-test :as bt]
         '[boot-mvn.core :refer [mvn]]
         '[clojure.java.io :as io]
				 #_'[spec-coverage.boot :as cover])

(deftask deps
  [])

(defn test* [test-fn]
  (test-fn :filters '[(-> % meta :integration not)]))

(deftask test
  []
  (test* bt/test))

#_
(deftask spec-coverage
  []
  (test* cover/spec-coverage))
