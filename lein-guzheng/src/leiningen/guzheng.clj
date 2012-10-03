(ns leiningen.guzheng
  (:use clojure.pprint)
  (:require leiningen.test
            [leiningen.core.project :as project])
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
  (let [[nses [_ & subtask]] (split-with #(not= "--" %) args)]
    [nses subtask]))

(defn update-project-dependencies
  [project deps]
  (let [profile-name (-> (gensym) name keyword)
        added-profile (project/add-profiles project
                                            {profile-name deps})
        merged-profile (project/merge-profiles added-profile [profile-name])]
    merged-profile))

(defn guzheng
  "Takes a list of namespaces followed by -- and
  another leiningen task and executes that task
  with the given namespaces instrumented."
  [project & args]
  (let [[nses [subtask & sub-args]] (split-ns-subtask args)
        project (-> project
                  (update-project-dependencies
                    {:dependencies [guzheng-version]
                     :sleight {:guzheng {:transforms ['guzheng.core/instrument]
                                         :namespaces (vec nses)}}}))
        task-args (concat [":guzheng" subtask] sub-args)]
    (binding [leiningen.test/*exit-after-tests* false]
      (eval/apply-task "sleight" project task-args))))
