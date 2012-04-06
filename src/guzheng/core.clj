(ns guzheng.core
  (:use [clojure [walk :only [postwalk]]]))

(defn str->reader
  "Converts a string to a java.io.Reader"
  [s]
  (-> s
    java.io.StringReader.
    clojure.lang.LineNumberingPushbackReader.))

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

(defn trace-ifs
  [f ast]
  (postwalk #(if (and (seqable? %) (= (first %) 'if)) (f %) %) ast))

;(require 'clojure.pprint)

(defn print-trace-if
  [ast]
  `(let [x# ~(second ast)]
     (println (str "if statement on line "
                   ~(-> ast meta :line)
                   " is " x#))
     (if x# ~@(nnext ast))))

(def ^:dynamic *trace-id*)
(defn branchdetect-if
  "Takes an ast of an if and returns
  the transformed AST, having added its
  info to the *trace-atom* and adding
  a call to update its data from the
  *trace-atom* when the branch gets executed."
  [a ast]
  (let [id @*trace-id*]
    (swap! *trace-id* inc)
    (clojure.pprint/pprint [:ast :is ast])
    `(do
       (when-not (get @~a ~id)
         (swap! ~a
                assoc
                ~id
                {:line ~(-> ast meta :line)
                 :ast '~ast
                 :lhs 0
                 :rhs 0})) 
       (if ~(second ast)
         (let [lhs# (get-in @~a [~id :lhs])]
           (swap! ~a
                  assoc-in
                  [~id :lhs]
                  (inc lhs#))
           ~(nth ast 2))   
         (let [rhs# (get-in @~a [~id :rhs])]
           (swap! ~a
                  assoc-in
                  [~id :rhs]
                  (inc rhs#))
           ~(nth ast 3))))))

(defn trace-if-branches
  "a is an atom that'll contain the instrumentation
  data."
  [a ast]
  (binding [*trace-id* (atom 0)]
    (let [trace-atom (gensym)
          new-ast
          (postwalk 
            (fn [node]
              (if (seqable? node)
                (condp = (first node)
                  'if (branchdetect-if trace-atom node)
;                  'defn (concat node
;                                `( (clojure.pprint/pprint
;                                   [:trace ~trace-atom])))
                  node)
                node))
            ast)
          new-ast (concat new-ast `((defn ~'trace-result [] 
                                      [:trace ~trace-atom])))]
      (clojure.pprint/pprint new-ast)
      `(let [~trace-atom (atom {})]
         ~new-ast))))

(def a (atom {}))
(instrument "
            ;go fred!
            (ns fred.rules)
            (defn hello-world [] (println \"hello world2\")))
            "
            (partial trace-if-branches a))

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
