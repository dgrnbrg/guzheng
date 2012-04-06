(ns leiningen.guzheng
  (:use [leiningen.compile :only [eval-in-project]])
  (:use guzheng.core))

(defn guzheng [project & nses]
  (eval-in-project
    project `(do
               (guzheng.core/run-test-instrumented
                 guzheng.core/trace-if-branches
                 ['guzheng.sample]
                 'guzheng.test.core)
               (guzheng.core/report-missing-coverage))
    `(require 'guzheng.core)))
