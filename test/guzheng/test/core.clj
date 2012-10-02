(ns guzheng.test.core
  (:use [clojure.java.shell :only [sh]])
  (:use [clojure test pprint]))

(def expected-output
  "in ns foo.core: arity [] is not covered in \"defn goodbye\" on line 7
in ns foo.core: false branch is not covered in \"if\" on line 16
in ns foo.core: arity [] is not covered in \"defn goodbye3\" on line 21
in ns foo.core: arity [a x] is not covered in \"defn goodbye3\" on line 21
in ns foo.core: false branch is not covered in \"if\" on line 31
in ns foo.core: 2 is not covered in \"case\" on line 37
in ns foo.core: default case is not covered in \"case\" on line 43
in ns foo.core: 1 is not covered in \"case\" on line 43
in ns foo.core: :else is not covered in \"cond\" on line 50
in ns foo.core: (* 2 2) is not covered in \"condp\" on line 56
in ns foo.core: weird-string is not covered in \"condp\" on line 62
in ns foo.core: (* 2 2) is not covered in \"condp\" on line 62
in ns foo.core: last clause is not covered in \"condp\" on line 62
in ns foo.core: body is not covered in \"when\" on line 70
in ns foo.core: body is not covered in \"when-not\" on line 74
in ns foo.core: body is not covered in \"when-let\" on line 76
in ns foo.core: body is not covered in \"when\" on line 80
in ns foo.core: true branch is not covered in \"if-not\" on line 82
in ns foo.core: false branch is not covered in \"if-let\" on line 85
in ns foo.core: arity [a b c] is not covered in \"defn- test-private-fn\" on line 89
in ns foo.core: body is not covered in \"delay\" on line 95
in ns foo.core: arity [x] is not covered in \"defmethod for dispatch value :a\" on line 104
")

(deftest test-lein1
  (let [{:keys [err out exit]}
        (sh "lein" "clean," "deps," "version" :dir "test-project")]
    (println out)
    (is (re-find #"Leiningen 1\." out))
    (is (= 0 exit))) 
  (let [{:keys [err out exit]}
        (sh "lein" "guzheng" "foo.core" "--" "test" :dir "test-project")]
    (println out)
    (is (= 0 exit))
    (is (.endsWith out expected-output))))

(deftest test-lein2
  (let [{:keys [err out exit]}
        (sh "lein2" "do" "clean," "deps," "version" :dir "test-project")]
    (println out)
    (is (re-find #"Leiningen 2\." out))
    (is (= 0 exit))) 
  (let [{:keys [err out exit ] :as lein2-output}
        (sh "lein2" "guzheng" "foo.core" "--" "test" :dir "test-project")]
    (println lein2-output)
    (flush)
    (is (= 0 exit))
    (is (.endsWith out expected-output))))

(deftest midje-lein2
  (let [{:keys [err out exit]}
        (sh "lein2" "do" "clean," "deps," "version" :dir "test-project")]
    (println out)
    (is (re-find #"Leiningen 2\." out))
    (is (= 0 exit))) 
  (let [{:keys [err out exit ] :as lein2-midje-output}
        (sh "lein2" "guzheng" "foo.core" "--" "midje" :dir "test-project")]
    (println lein2-midje-output)
    (flush)
    (is (= 0 exit))
    (is (.endsWith out expected-output))))
