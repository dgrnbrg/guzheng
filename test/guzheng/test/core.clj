(ns guzheng.test.core
  (:use [guzheng sample])
  (:use [clojure.test]))

(deftest test-sample
  (is (= 22 (do-hello))))
