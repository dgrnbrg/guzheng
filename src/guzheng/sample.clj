(ns guzheng.sample)

(defn do-hello
  []
  (println "hello")
  (if true (println 22) "no soup"))

(defn do-condp
  [x]
  (condp = x
     "hi" 22
    22 "blah"
    "you win!"))

(defn do-condp2
  [x]
  (condp = x
     "hi" 22
    22 "blah"))

(defn do-cond
  [x]
  (cond
    (= x "hi") 22
    (= x 22) "blah"
    :else "you win!"))

(defn uncalled-for [] (if true (println "You should never see this") ))

(defn defn-multi-arity
  ([] "hi")
  ([x] "blah"))

(def foo (fn [x] (inc x)))

(def fn-multi-arity (fn
  ([] "hi")
  ([x] "blah")))
