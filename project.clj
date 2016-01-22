(defproject sparkfund/sails-forth "0.1.2"
  :description "A fully-typed salesforce library"
  :url "http://github.com/sparkfund/sails-forth"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cheshire "5.5.0"]
                 [clj-http "2.0.0"]
                 [clj-time "0.11.0"]
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.18"]]
  :repl-options {:init-ns sails-forth.repl}
  :test-selectors {:integration :integration
                   :all (constantly true)
                   :default (complement :integration)})
