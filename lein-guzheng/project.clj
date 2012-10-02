(defproject lein-guzheng "0.4.3"
  :description "This creates a lein plugin for guzheng"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :eval-in-leiningen true
  :dependencies [[guzheng ~(nth (read-string (slurp "../project.clj")) 2)]
                 [bultitude "0.1.5"]
                 [lein-sleight "0.2.0-SNAPSHOT"]
                 [robert/hooke "1.1.3"]
                 [leinjacker "0.3.0"]]
  :dev-dependencies [[lein-clojars "0.6.0"]])
