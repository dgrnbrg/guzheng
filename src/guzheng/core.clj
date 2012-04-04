(ns guzheng.core
  (:use [clojure [walk :only [postwalk]]]))

(defn str->reader
  "Converts a string to a java.io.Reader"
  [s]
  (-> s
    java.io.StringReader.
    java.io.PushbackReader.))

(defn seqable?
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))

(defn instrument
  "Takes a string, parses it and passes it to
  the instrumentation fn, which then returns the
  AST to be evaluated. Then the string is evaluated."
  [s f]
  (let [s-wrapped (str "(do\n" s "\n)")
        reader (str->reader s-wrapped)
        ast-unmodified (read reader)
        ast-instrumented (f ast-unmodified)
        old-ns (ns-name *ns*)
        result (eval ast-instrumented)]
    (in-ns old-ns)
    result))

(instrument "
            ;go fred!
            (ns fred.rules)
            (defn hello-world [] (println \"hello world2\")))
            "
            (partial postwalk
                     (fn [node]
                       (if (and (seqable? node)
                                (= (first node) 'println))
                         (concat '(do (println "calling println"))
                                 node)
                         node))))

#_(alter-var-root #'clojure.core/load
                (fn [old-load]
                  (fn custom-load
                    [& paths]
                    (doseq [^String path paths]
                      (let [^String path (if (.startsWith path "/")
                                             path
                                             (str (#'clojure.core/root-directory (ns-name *ns*))
                                                  \/ path))
                            core (create-ns 'clojure.core)]
                        (when #'clojure.core/*loading-verbosely*
                          (printf "(clojure.core/load \"%s\")\n" path)
                          (flush))
                        (#'clojure.core/check-cyclic-dependency path)
                        (when-not (= path (first (resolve core '*pending-paths*)))
                          (binding [clojure.core/*pending-paths*
                                    (conj (resolve core '*pending-paths*) path)]
                            (-> (clojure.lang.RT/resourceAsStream
                                    (clojure.lang.RT/baseLoader)
                                    (.substring path 1))
                              java.io.InputStreamReader.
                              java.io.StringReader.
                              .toString
                              (instrument
                                (partial postwalk
                                         (fn [node]
                                           (if (and (seqable? node)
                                                    (= (first node) 'println))
                                             (concat '(do (println "calling println"))
                                                     node)
                                             node)))) 
                              )))))
                    )))

(defn run-test-instrumented
  [f instrumented-nses & nses]
  (doseq [ns instrumented-nses]
    (-> clojure.lang.Compiler
      .getClassLoader
      (.getResourceAsStream (str (.replace (name ns) "." "/") ".clj"))
      java.io.InputStreamReader.
      slurp
      (instrument f)))
    (apply clojure.test/run-tests nses)) 

;following is sample usage
#_(guzheng.core/run-test-instrumented 
         (partial clojure.walk/postwalk
                  (fn [node]
                    (if (and (seqable? node)
                             (= (first node) 'println))
                      (concat `(do (println (str "calling println with args: " '~(rest node))))
                              node)
                      node)))
         ['guzheng.sample]
         'guzheng.test.core)
(comment
  Need to set clojure.core/load to be a function that loads the given resource.
  )
