(defproject sparkfund/sails-forth "0.6.0-alpha1"
  :description "A Salesforce library"
  :url "http://github.com/sparkfund/sails-forth"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cheshire "5.5.0"]
                 [clj-http "2.0.0"]
                 [clj-time "0.11.0"]
                 [com.github.jsqlparser/jsqlparser "0.9.5"]
                 [org.clojure/clojure "1.9.0-alpha17"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :repl-options {:init-ns sails-forth.repl}
  :test-selectors {:integration :integration
                   :all (constantly true)
                   :default (complement :integration)})
