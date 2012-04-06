(ns guzheng.core
  (:use [clojure [walk :only [postwalk walk prewalk]]]))

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
  (let [s-wrapped (str "(do " s ")")
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
(def ^:dynamic *initial-registrations*)

(defn register-branch
  "Register a branch given a list of
  branch ids"
  [trace-atom node & bs]
  (let [branch-map (zipmap bs (repeat 0))
        ast (:body node)]
    (swap! *trace-id* inc)
    (swap! *initial-registrations*
           conj
           `(swap! ~trace-atom
                   assoc
                   ~(identity @*trace-id*) 
                   (merge
                     {:line ~(:line node) 
                      :ast '~ast}
                     ~branch-map)))))

(defn trace-branch
  "Takes a fragment of an ast branch
  and a branch id and generates
  a fragment that traces that branch."
  [trace-atom branch id]
  `(let [count# (get-in @~trace-atom
                        [~(identity @*trace-id*) ~id])]
     (println "hello from a teply wizard")
     (swap! ~trace-atom
            assoc-in
            [~(identity @*trace-id*) ~id]
            (inc count#))
     ~branch))   

(defn branchdetect-if
  "Takes an ast of an if and returns
  the transformed AST, having added its
  info to the *trace-atom* and adding
  a call to update its data from the
  *trace-atom* when the branch gets executed."
  [trace-atom node]
  (register-branch trace-atom node :lhs :rhs)
  (let [ast (:body node)]
    `(if ~(first ast)
       ~(trace-branch trace-atom
                      (nth ast 1)
                      :lhs)
       ~(trace-branch trace-atom
                      (nth ast 2)
                      :rhs))))

(defn branchdetect-cond
  [trace-atom node]
  (let [clauses (partition 2 (:body node))
        branch-ids (range (count clauses))
        conditions (map first clauses)
        branches (map second clauses)]
    (apply register-branch trace-atom node branch-ids) 
    `(cond ~@(interleave
               conditions
               (map (partial trace-branch trace-atom)
                    branches
                    branch-ids)))))

(defn branchdetect-condp
  [trace-atom node]
  (let [clauses (partition 2 2 [] (nthnext node 2))
        final-clause (last clauses)
        final-clause (if (= 1 (count final-clause))
                       final-clause
                       nil)
        branch-ids (range (count clauses))
        clauses (if final-clause
                  (butlast clauses)
                  clauses)
        conditions (map first clauses)
        branches (map second clauses)]
  (apply register-branch trace-atom node branch-ids) 
    (let [without-final
          `(condp ~(first (:body node)) ~(second (:body node))
             ~@(interleave
                 conditions
                 (map (partial trace-branch trace-atom)
                      branches
                      branch-ids)))]
      (if final-clause
        (concat without-final
                (trace-branch trace-atom
                              final-clause
                              (last branch-ids)))
        without-final))))


(def main-trace-atom (atom {}))
;TODO: access internal atom
(defn trace-if-branches
  "a is an atom that'll contain the instrumentation
  data."
  [ast] 
  (binding [*trace-id* (atom 0)
            *initial-registrations* (atom [])]
    (let [trace-atom 'guzheng.core/main-trace-atom
          ast
          (prewalk
            (fn [node]
              (if (seqable? node)
                (let [line (-> node meta :line)]
                  (condp = (first node)
                    'if {::trace true
                         :type :if
                         :line line
                         :body (rest node)}
                    'cond {::trace true
                           :type :cond
                           :line line
                           :body (rest node)} 
                    'condp {::trace true
                            :type :condp
                            :line line
                            :body (rest node)} 
                    ;'fn {::trace true
                    ;     :type :fn
                    ;     :line line
                    ;     :body (rest node)}
                    ;'defn {::trace true
                    ;       :type :defn
                    ;       :line line
                    ;       :body (rest node)}
                    node))
                node))
            ast)
          new-ast
          (postwalk 
            (fn [node]
              (if (::trace node)
                (condp = (:type node)
                  :if (branchdetect-if
                        trace-atom node)
                  :cond (branchdetect-cond
                          trace-atom node)
                  :condp (branchdetect-condp
                           trace-atom node)
                  ;:fn (branchdetect-fn
                  ;      trace-atom node)
                  ;:defn (branchdetect-fn
                  ;        trace-atom node)
                  (throw (RuntimeException.
                           (str "Cannot trace "
                                (:type node)))))
                node))
            ast)
          new-ast (concat
                     new-ast
                     @*initial-registrations*)]
      ;(clojure.pprint/pprint *initial-registrations*)
      (clojure.pprint/pprint new-ast) 
      new-ast
      )))

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
         trace-if-branches
         ['guzheng.sample]
         'guzheng.test.core)
;check the contents of guzheng.core/main-trace-atom
;
(comment
  Need to set clojure.core/load to be a function that loads the given resource.
  )
