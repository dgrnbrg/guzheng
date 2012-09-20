(ns guzheng.test.core
  (:use [guzheng sample])
  (:use [clojure.test]))

(deftest test-sample
  (is (= 22 (do-hello))))

(deftest test-sample2
  (is (= nil (do-hello))))

(deftest cond-test
  (is (= (do-cond "hi") 22))
  (is (= (do-cond nil) "you win!")))

(deftest condp-test
  (is (= (do-condp "hi") 22))
  (is (= (do-condp nil) "you win!")) 
  (is (= (do-condp2 "hi") 22)))

(deftest defn-multi-arity-test
  (is (= (defn-multi-arity) "hi")))

