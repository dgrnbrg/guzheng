(ns guzheng.core
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
      ))

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

(def ^:dynamic *trace-id*)
(def ^:dynamic *initial-registrations*)
(def ^:dynamic *current-branch* nil)
(def ^:dynamic *parent-branch* nil)
(def ^:dynamic *initialize-main-traces* false)
(def main-trace-atom (atom {}))

(defn register-branch
  "Register a branch given a list of
  branch ids"
  [trace-atom node & bs]
  (let [branch-map (zipmap bs (repeat 0))
        ast (:body node)]
    (when *initialize-main-traces*
      (swap! main-trace-atom
             assoc
             *current-branch* 
             (merge
               {:line (:line node) 
                :type (:type node)
                :ns (ns-name *ns*)
                :ast ast
                :parent *parent-branch*}
               branch-map))) 
    *current-branch*))

(defn trace-branch
  "Takes a fragment of an ast branch
  and a branch id and generates
  a fragment that traces that branch."
  [trace-atom branch id branch-id]
  `(do 
     (swap! ~trace-atom
            update-in
            [~id ~branch-id]
            inc)
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

(defn extract-fn-arities
  "Takes a fn body and extracts the single or multiple arity form."
  [fn-body]
  (let [[name+metadata bodies] (split-with #(not (or (seq? %)
                                                     (vector? %))) fn-body)
        fixup-single-arity (if (vector? (first bodies))
                             (list bodies)
                             bodies)]
    [name+metadata fixup-single-arity]))

(defn branchdetect-fn
  [trace-atom fn-decl node]
  (let [ast (:body node)
        [fn-name+meta fn-bodies] (extract-fn-arities ast) 
        branch-ids (map (comp count first) fn-bodies) 
        id (apply register-branch trace-atom node branch-ids)]
    (cons fn-decl (concat fn-name+meta
                        (map (fn [body branch-id]
                               (list (first body) ;arg list
                                     (trace-branch trace-atom (cons `do (rest body))
                                                   id branch-id)))
                             fn-bodies branch-ids)))))
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

(defmulti analyze-node
  "Takes a node and returns either the
  unmodified node or the node having been instrumented."
  (fn [node line trace-atom]
    (try
      (-> node first name keyword)
      (catch Exception _
        :terminal))))

(defmethod analyze-node :default
  [node line trace-atom]
  node)

(defmethod analyze-node :terminal
  [node line trace-atom]
  node)

(defmethod analyze-node :if
  [node line trace-atom]
  (branchdetect-if
    trace-atom
    {:type :if
     :line line
     :body (if (= 3 (count node))
             (conj (rest node) nil)
             (rest node))}))

(defmethod analyze-node :cond
  [node line trace-atom]
  (branchdetect-cond 
    trace-atom
    {:type :cond 
     :line line
     :body (rest node)}))

(defmethod analyze-node :condp
  [node line trace-atom]
  (branchdetect-condp
    trace-atom
    {:type :condp
     :line line
     :body (rest node)}))


(defmethod analyze-node :defn
  [node line trace-atom]
  (branchdetect-fn 
    trace-atom
    `defn
    {:type :defn 
     :line line
     :body (rest node)}))

(defmethod analyze-node :fn
  [node line trace-atom]
  (branchdetect-fn 
    trace-atom
    `fn
    {:type :fn 
     :line line
     :body (rest node)}))

(defn walk-trace-branches
  [node trace-atom]
  (if-not (seqable? node)
    node
    (binding [*parent-branch* *current-branch*
              *current-branch* (swap! *trace-id* inc)]
     (let [line (-> node meta :line)
          node (if (and (seqable? node)
                        ;TODO: determine if the following line is needed
                        ;for tracing macros
                        #_(not= 'quote (first node))
                        )
                 (doall (map #(walk-trace-branches % trace-atom) node))
                 node)]
      (analyze-node node line trace-atom)))))

(defn trace-if-branches
  "Instruments the ast to trace all conditional branches (cond, condp, fn, defn, and if).
  
  If passed the setting :reload, will not include the code that resets the counters.
  "
  [ast & settings] 
  (binding [*trace-id* (atom 0)
            *initial-registrations* (atom [])]
    (let [trace-atom 'guzheng.core/main-trace-atom
          transformed-ast (binding [*initialize-main-traces*
                                    (not (some #{:reload} settings))]
                            (walk-trace-branches ast trace-atom))
          new-ast transformed-ast]
      ;(clojure.pprint/pprint *initial-registrations*)
      ;(clojure.pprint/pprint new-ast) 
      new-ast
      )))

(defn instrument-ns
  "Takes an ns and a form for the instrumnetation
  function and returns a form to be evaluated that
  will have that ns be instrumented."
  [ns f]
  (let [path (path-for ns)]
    (println (str "instrumenting " path))
    (flush)
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

(defn report-missing-coverage
  []
  (let [results @main-trace-atom
        errors-so-far (atom #{})]
    (doseq [[id {:keys [type ast line ns parent] :as data}] results]
      (letfn [(report [msg stmt]
                (when-not (contains? @errors-so-far parent)
                  (println (str "in ns " ns ": "
                                msg
                                " is not covered in \""
                                stmt
                                "\" on line " line)))
                (swap! errors-so-far conj id)) 
              (report-fn [fn-decl]
                (let [[_ bodies] (extract-fn-arities ast)
                      arities (map (comp (juxt identity count)
                                         first) bodies)]
                  (doseq [[args arity] arities]
                    (when (zero? (get data arity))
                      (report (str "arity " args)
                              (str fn-decl \space (first ast)))))))] 
        (condp = type
          :if (do
                (when (zero? (:lhs data))
                  (report "true branch" "if"))
                (when (zero? (:rhs data))
                  (report "false branch" "if")))
          :defn (report-fn "defn") 
          :fn (report-fn "fn")
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
