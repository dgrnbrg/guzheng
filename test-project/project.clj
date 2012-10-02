(defproject test-project "1.0.0-SNAPSHOT" 
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [guzheng "1.2.5"]
                 [sleight "0.2.0-SNAPSHOT"]]

  ;; Lein 1
  :dev-dependencies [[lein-guzheng ~(nth (read-string (slurp "../lein-guzheng/project.clj")) 2)]] 

  ;; Lein 2
  :plugins [[lein-guzheng ~(nth (read-string (slurp "../lein-guzheng/project.clj")) 2) ]
            [lein-midje "2.0.0-SNAPSHOT"]
            [leinjacker "0.3.0"]])
