(ns foo.core)

(defn hello-world
  []
  "hello world")

(defn goodbye
  []
  (if true
    "lol"
    "bol")
  "goodbye")

(defn goodbye2
  []
  (if true
    "lol"
    "bol")
  "goodbye")

(defn goodbye3
  ([]
   "hi")
  ([h]
   "bye")
  ([a x]
   "thrice"))

(defn branches
  [x]
  (if x
    (println "hello world")
    (println "never happens")))

(defn test-case1
  [x]
  (case x
    1 "hi"
    2 "bye"))

(defn test-case2
  [x]
  (case x
    1 "hi"
    2 "bye"
    "garbage"))

(defn test-cond
  [x y]
  (cond
    true 1
    :else 2))

(defn test-condp1
  []
  (condp = 3
    (+ 1 2) "hi"
    (* 2 2) "lol"))

(defn test-condp2
  []
  (condp = 3
    (+ 1 2) "hi"
    "weird-string" :>> (fn [my-result] "hi")
    (* 2 2) "lol"
    "no"))

(defn test-other-conditions
  []
  (when false
    (println "fail the test!1"))
  (when true
    (println "fail the test!2"))
  (when-not true
    (println "fail the test!3"))
  (when-let [x false]
    (println "fail the test!4"))
  (when-let [x true]
    22)
  (let [test-me (when false 22)]
    test-me)
  (if-not true
    "hi"
    "bye")
  (if-let [x 22]
    (* x x)
    -77))

(defn- test-private-fn
  [a b c]
  nil)

(defn test-delay
  []
  (let [x (delay
            (* 7 8)
            (+ 3 1))
        y (delay 2)]
    @y))

(defmulti test-multi
  identity)

(defmethod test-multi :a
  [x]
  "got to a")

(defmethod test-multi :b
  [x]
  "got to b")
