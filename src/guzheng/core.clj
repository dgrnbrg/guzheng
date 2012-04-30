(ns guzheng.core
  (:use [clojure [walk :only [postwalk walk prewalk]]])
  (:use [bultitude.core :only [path-for]])
  (:require [clojure pprint test]))

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
                      :type ~(:type node)
                      :ns (ns-name *ns*)
                      :ast '~ast}
                     ~branch-map)))
    @*trace-id*))

(defn trace-branch
  "Takes a fragment of an ast branch
  and a branch id and generates
  a fragment that traces that branch."
  [trace-atom branch id branch-id]
  `(let [count# (get-in @~trace-atom
                        [~id ~branch-id])]
     (swap! ~trace-atom
            assoc-in
            [~id ~branch-id]
            (inc count#))
     ~branch))   

(defn branchdetect-if
  "Takes an ast of an if and returns
  the transformed AST, having added its
  info to the *trace-atom* and adding
  a call to update its data from the
  *trace-atom* when the branch gets executed."
  [trace-atom node]
  (let [ast (:body node)
        id (register-branch trace-atom node :lhs :rhs)]
    `(if ~(first ast)
       ~(trace-branch trace-atom
                      (nth ast 1)
                      id
                      :lhs)
       ~(trace-branch trace-atom
                      (nth ast 2)
                      id
                      :rhs))))

(defn index-of-first
  [pred s]
  (loop [i 0 s (seq s)]
    (if (or (empty? s) (pred (first s)))
      i
      (recur (inc i) (next s)))))

(defn branchdetect-defn
  [trace-atom node]
  (let [ast (:body node)
        [name-args body] (split-at (inc (index-of-first vector? ast)) ast)
        id (register-branch trace-atom node :main)]
    `(defn ~@name-args
       ~(trace-branch trace-atom
                       `(do ~@body)
                       id
                       :main))))

(defn branchdetect-fn
  [trace-atom node]
  (let [ast (:body node)
        [name-args body] (split-at (inc (index-of-first vector? ast)) ast)
        id (register-branch trace-atom node :main)]
    `(fn ~@name-args
       ~(trace-branch trace-atom
                       `(do ~@body)
                       id
                       :main))))

(defn branchdetect-cond
  [trace-atom node]
  (let [clauses (partition 2 (:body node))
        branch-ids (range (count clauses))
        conditions (map first clauses)
        branches (map second clauses)
        id (apply register-branch
                  trace-atom node branch-ids)]
    `(cond ~@(interleave
               conditions
               (map (partial trace-branch trace-atom)
                    branches
                    (repeat id)
                    branch-ids)))))

(defn branchdetect-condp
  [trace-atom node]
  (let [clauses (partition 2 2 [] (nthnext (:body node) 2))
        final-clause (last clauses)
        final-clause (if (= 1 (count final-clause))
                       (first final-clause) 
                       nil)
        branch-ids (range (count clauses))
        clauses (if final-clause
                  (butlast clauses)
                  clauses)
        conditions (map first clauses)
        branches (map second clauses)
        id (apply register-branch
                  trace-atom node branch-ids)]
    (let [without-final
          `(condp ~(first (:body node)) ~(second (:body node))
             ~@(interleave
                 conditions
                 (map (partial trace-branch trace-atom)
                      branches
                      (repeat id)
                      branch-ids))
             )]
      (if final-clause
        (concat without-final
                (list (trace-branch trace-atom
                                    final-clause
                                    id
                                    (last branch-ids))))
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
                         :body (if (= 3 (count node))
                                 (conj (rest node) nil)
                                 (rest node))}  
                    'cond {::trace true
                           :type :cond
                           :line line
                           :body (rest node)} 
                    'condp {::trace true
                            :type :condp
                            :line line
                            :body (rest node)} 
                    'fn {::trace true
                         :type :fn
                         :line line
                         :body (rest node)}
                    'defn {::trace true
                           :type :defn
                           :line line
                           :body (rest node)}
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
                  :fn (branchdetect-fn
                        trace-atom node)
                  :defn (branchdetect-defn
                          trace-atom node)
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

(defn instrument-ns
  "Takes an ns and a form for the instrumnetation
  function and returns a form to be evaluated that
  will have that ns be instrumented."
  [ns f]
  (let [path (path-for ns)]
    (println (str "instrumenting " path))
    (-> clojure.lang.Compiler
      .getClassLoader
      (.getResourceAsStream path)
      java.io.InputStreamReader.
      slurp
      (instrument f))))

(defn instrument-nses
  [f nses]
  (doseq [ns nses]
    (instrument-ns ns f)))

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

(defn report-missing-coverage
  []
  (let [results (vals @main-trace-atom)]
    (doseq [{:keys [type ast line ns] :as data} results]
      (letfn [(report [msg stmt]
                (println (str "in ns " ns ": "
                              msg
                              " is not covered in \""
                              stmt
                              "\" on line " line)))] 
       (condp = type
        :if (do
              (when (zero? (:lhs data))
                (report "true branch" "if"))
              (when (zero? (:rhs data))
                (report "false branch" "if")))
         :defn (do
               (when (zero? (:main data))
                 (report (first ast) "defn")))
         :fn (do
               (when (zero? (:main data))
                 (report (if (string? (first ast))
                           (first ast)
                           "body") "fn")))
         :cond (let [clauses (keep-indexed
                               vector
                               (partition 2 ast))]
                 (doseq [[id clause] clauses]
                   (when (zero? (get data id))
                     (report (first clause) "cond"))))
         :condp (let [ast (drop 2 ast)
                      clauses (keep-indexed
                               vector
                               (partition 2 ast))
                      final (if (odd? (count ast))
                              [(-> (count ast)
                                 dec
                                 (/ 2))
                               (vector "last clause"
                                       (last ast))] 
                              nil)
                      clauses (if-not (nil? final)
                                (conj clauses final)
                                clauses)]
                 (doseq [[id clause] clauses]
                   (when (zero? (get data id))
                     (report (first clause) "condp"))))
         nil)))))

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

;TODO: no good story for tracking fns written as #(inc %)
