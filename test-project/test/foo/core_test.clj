(ns foo.core-test
  (:use clojure.test) 
  (:use foo.core))

(deftest a-test
  (is (= (foo.core/hello-world) "hello world"))
  (is (= (foo.core/goodbye3 nil) "bye"))
  (is (nil? (foo.core/branches true)))
  (is (= (foo.core/goodbye2) "goodbye"))
  (is (= (foo.core/test-case2 2) "bye")) 
  (is (= (foo.core/test-case1 1) "hi"))
  (is (= (foo.core/test-cond nil nil) 1))
  (is (= (foo.core/test-condp1) "hi"))
  (is (= (foo.core/test-condp2) "hi"))
  (is (= (foo.core/test-other-conditions) (* 22 22)))
  (is (= (foo.core/test-multi :b) "got to b"))
  (is (= (foo.core/test-delay) 2)))
