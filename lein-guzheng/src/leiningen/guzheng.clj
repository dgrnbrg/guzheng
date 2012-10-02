(ns leiningen.guzheng
  (:use clojure.pprint)
  (:require leiningen.test)
  (:require [leiningen.sleight :as sleight])
  (:require [leinjacker.eval :as eval])
  (:require [clojure.java.io :as io]))

(def ^ {:private true :const :true}
  guzheng-version
  ['guzheng "1.2.5"])

(defn- split-ns-subtask
  "Takes the namespaces followed by \"--\" followed
  by the leiningen command to run with an instrumented
  eval-in-project."
  [args]
  (let [[nses subtask] (split-with #(not= "--" %) args)]
    [nses subtask]))

(defn guzheng
  "Takes a list of namespaces followed by -- and
  another leiningen task and executes that task
  with the given namespaces instrumented."
  [project & args]
  (let [[nses [_ subtask & sub-args]] (split-ns-subtask args)
        project (-> project
                  (update-in [:dependencies] conj guzheng-version)
                  (assoc-in [:sleight :guzheng]
                             {:transforms ['guzheng.core/instrument]
                              :namespaces (vec nses)}))
        task-args (concat [":guzheng" subtask] sub-args)]
    (pprint project)
    (println "calling sleight with" task-args)
    (binding [leiningen.test/*exit-after-tests* false]
      (eval/apply-task "sleight" project task-args))))
