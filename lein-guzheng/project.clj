(defproject lein-guzheng "0.4.4-SNAPSHOT"
  :description "This creates a lein plugin for guzheng"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :eval-in-leiningen true
  :dependencies [[lein-sleight "0.2.0-SNAPSHOT"]
                 [leinjacker "[0.3.0,)"]]
  :plugins [[lein-clojars "0.6.0"]])
