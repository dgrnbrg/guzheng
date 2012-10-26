(ns guzheng.core
  "TODO: when-first, for, lazy-seq, lazy-cat, definline, defmacro, everything w/ protocols.
  "
  (:refer-clojure :exclude [==])
  (:use [bultitude.core :only [path-for]]
        [clojure.core.logic]
        [clojure.string :only [join]])
  (:require [clojure pprint test]
            [sleight.core :as sleight]))

(defn str->reader
  "Converts a string to a java.io.Reader"
  [s]
  (-> s
    java.io.StringReader.
    clojure.lang.LineNumberingPushbackReader.))

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

(def trace-id (atom 0))
(def ^:dynamic *current-branch* nil)
(def ^:dynamic *parent-branch* nil)
(def ^:dynamic *initialize-main-traces* false)
(def main-trace-atom (atom {}))

(defn register-branch
  "Register a branch given a list of
  branch ids"
  [trace-atom node ns & bs]
  (let [branch-map (zipmap bs (repeat 0))
        ast (:body node)]
    (when *initialize-main-traces*
      (swap! main-trace-atom
             assoc
             *current-branch* 
             (merge
               {:line (:line node) 
                :type (:type node)
                :ns ns
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
   ([trace-atom if-form line ns node]
    (branchdetect-if
      trace-atom
      if-form
      (-> if-form name keyword)
      line
      ns
      node))
  ([trace-atom if-form if-type line ns node]
   (let [ast (if (= 3 (count node))
               (concat (rest node) [nil])
               (rest node)) 
         node {:type if-type
               :line line
               :body (if (= 3 (count node))
                       (conj (rest node) nil)
                       (rest node))}
         id (register-branch nil node ns :lhs :rhs)]
     (list if-form 
           (first ast) 
           (trace-branch trace-atom
                         (nth ast 1)
                         id
                         :lhs) 
           (trace-branch trace-atom
                         (nth ast 2)
                         id
                         :rhs)))))

(defn index-of-first
  [pred s]
  (loop [i 0 s (seq s)]
    (if (or (empty? s) (pred (first s)))
      i
      (recur (inc i) (next s)))))

(defn normalize-fntail
  "If the fntail isn't in the multi-arity form,
  makes it into the multiarity form. Otherwise returns
  the fntail.
  
  So `([a b c] ...)` will become `(([a b c] ...))`,
  and `(([a] ...) ([a b] ...))` will be unchanged."
  [fntail]
  (if (vector? (first fntail))
    (list fntail)
    fntail) )

(defn extract-fn-arities
  "Takes a fn body and extracts the single or multiple arity form."
  [fn-body]
  (let [[name+metadata bodies] (split-with #(not (or (seq? %)
                                                     (vector? %))) fn-body)
        fixup-single-arity (normalize-fntail bodies)]
    [name+metadata fixup-single-arity]))

(defn branchdetect-fn
  [trace-atom fn-decl ns node]
  (let [ast (:body node)
        [fn-name+meta fn-bodies] (extract-fn-arities ast) 
        branch-ids (range (count fn-bodies)) 
        id (apply register-branch trace-atom node ns branch-ids)]
    (cons fn-decl (concat fn-name+meta
                          (map (fn [body branch-id]
                                 (list (first body) ;arg list
                                       (trace-branch trace-atom (cons `do (rest body))
                                                     id branch-id)))
                               fn-bodies branch-ids)))))
(defn branchdetect-cond
  [trace-atom ns node]
  (let [clauses (partition 2 (:body node))
        branch-ids (range (count clauses))
        conditions (map first clauses)
        branches (map second clauses)
        id (apply register-branch
                  trace-atom node ns branch-ids)]
    `(cond ~@(interleave
               conditions
               (map (partial trace-branch trace-atom)
                    branches
                    (repeat id)
                    branch-ids)))))

(defne match-clauses [clauses parsed]
  ([() ()])
  ([[?last] p]
   (== [{:expr ?last
        :default true}]
       p))
  ([[?head :>> ?tail . ?rest] p]
   (fresh [parsed-tail]
          (match-clauses ?rest parsed-tail)
          (conso {:test ?head
                  :expr ?tail
                  :feed true}
                 parsed-tail
                 p)))
  ([[?head ?tail . ?rest] p]
   (!= ?tail :>>)
   (fresh [parsed-tail]
          (match-clauses ?rest parsed-tail)
          (conso {:test ?head
                  :expr ?tail}
                 parsed-tail
                 p))))

(defn branchdetect-condp
  [trace-atom ns node]
  (let [clauses (nthnext (:body node) 2)
        parsed-clauses (first (run 1 [q]
                                   (match-clauses clauses q)))
        branch-ids (range (count parsed-clauses))
        id (apply register-branch
                  trace-atom node ns branch-ids)]
    (concat (cons `condp (take 2 (:body node)))
            (mapcat
              (fn [{:keys [expr test feed default]} branch-id]
                (let [expr (trace-branch
                             trace-atom
                             expr
                             id
                             branch-id)]
                  (if-not default
                    (if feed
                      [test :>> expr]
                      [test expr])
                    [expr])))
              parsed-clauses
              branch-ids))))

(defmulti analyze-node
  "Takes a node and returns either the
  unmodified node or the node having been instrumented."
  (fn [node line ns trace-atom]
    (try
      (-> node first name keyword)
      (catch Exception _
        :terminal))))

(defmethod analyze-node :default
  [node line ns trace-atom]
  node)

(defmethod analyze-node :terminal
  [node line ns trace-atom]
  node)

(defmethod analyze-node :if-let
  [node line ns trace-atom]
  (branchdetect-if
    trace-atom
    `if-let
    line
    ns
    node))

(defmethod analyze-node :if-not
  [node line ns trace-atom]
  (branchdetect-if
    trace-atom
    `if-not
    line
    ns
    node))

(defmethod analyze-node :if
  [node line ns trace-atom]
  (branchdetect-if
    trace-atom
    `if
    line
    ns
    node))

(defmethod analyze-node :when
  [node line ns trace-atom]
  (branchdetect-if
    trace-atom
    `if
    :when
    line
    ns
    (macroexpand-1 node)))

(defmethod analyze-node :when-let
  [node line ns trace-atom]
  (branchdetect-if
    trace-atom
    `if-let
    :when-let
    line
    ns
    (macroexpand-1 node)))

(defmethod analyze-node :when-not
  [node line ns trace-atom]
  (branchdetect-if
    trace-atom
    `if
    :when-not
    line
    ns
    (macroexpand-1 node)))

(defmethod analyze-node :delay
  [node line ns trace-atom]
  (let [node {:type :delay
              :line line
              :body (cons `do (rest node))}
        id (register-branch trace-atom node ns :delayed)]
    (list `delay
          (trace-branch
            trace-atom
            (:body node)
            id
            :delayed))))

(defmethod analyze-node :cond
  [node line ns trace-atom]
  (branchdetect-cond 
    trace-atom
    ns
    {:type :cond 
     :line line
     :body (rest node)}))

(defmethod analyze-node :condp
  [node line ns trace-atom]
  (branchdetect-condp
    trace-atom
    ns
    {:type :condp
     :line line
     :body (rest node)}))

(defmethod analyze-node :case
  [node line ns trace-atom]
  (let [node {:type :case
              :line line
              :body (rest node)}
        [e & clauses] (:body node)
        clauses (partition 2 2 [] clauses)
        branch-ids (range (count clauses))
        last-clause (last clauses)
        default (when (= 1 (count last-clause))
                  last-clause)
        clauses (if default
                  (butlast clauses)
                  clauses)
        id (apply register-branch
                  trace-atom node ns branch-ids)]
    (concat
      (list `case e)
      (mapcat (fn [[constant expr] branch-id]
                [constant
                 (trace-branch
                   trace-atom
                   expr
                   id
                   branch-id)])
              clauses branch-ids)
      (when last-clause
        (list (trace-branch trace-atom
                            (first last-clause)
                            id
                            (last branch-ids))))))) 

(defmethod analyze-node :defn-
  [node line ns trace-atom]
  (branchdetect-fn 
    trace-atom
    `defn-
    ns
    {:type :defn-
     :line line
     :body (rest node)}))

(defmethod analyze-node :defn
  [node line ns trace-atom]
  (branchdetect-fn 
    trace-atom
    `defn
    ns
    {:type :defn 
     :line line
     :body (rest node)}))

(defmethod analyze-node :fn
  [node line ns trace-atom]
  (branchdetect-fn 
    trace-atom
    `fn
    ns
    {:type :fn 
     :line line
     :body (rest node)}))

(defmethod analyze-node :defmethod
  [node line ns trace-atom]
  (let [[d-m multifn dispatch-val & fn-tail] node
        node {:type :defmethod
              :line line
              :body (rest node)}
        fn-tails (normalize-fntail fn-tail)
        branch-ids (range (count fn-tails))
        id (apply register-branch trace-atom node ns branch-ids)]
    (list* d-m multifn dispatch-val
           (map (fn [[args & body] branch-id]
                  (list args
                        (trace-branch trace-atom (cons `do body)
                                      id branch-id)))
                fn-tails
                branch-ids))))

(defn preservative-walk
  [obj f]
  (cond
    (list? obj) (doall (map f obj))
    (vector? obj) (into [] (map f obj))
    (set? obj) (into #{} (map f obj))
    (map? obj) (into {} (map (fn [[k v]]
                               [(f k) (f v)])
                             obj))
    :else obj))

(defn walk-trace-branches
  [node trace-atom ns]
  ;TODO: perhaps this if is not needed
  (if-not (->> [list? vector? map? set?]
            (map #(% node)) 
            (some identity))
    node
    (binding [*parent-branch* *current-branch*
              *current-branch* (swap! trace-id inc)]
     (let [line (-> node meta :line)
          node (preservative-walk
                 node
                 #(walk-trace-branches % trace-atom ns))]
      (analyze-node node line ns trace-atom)))))

(defn get-ns-name
  "Searches for the `ns` macro in the given AST and returns
  the namespace for the given AST."
  [ast]
  (cond
    (= (first ast) `do)
    (get-ns-name (second ast))
    (= (name (first ast)) "ns") 
    (second ast)
    :else
    'user))

(defn trace-if-branches
  "Instruments the ast to trace all conditional branches (cond, condp, fn, defn, and if).
  
  If passed the setting :reload, will not include the code that resets the counters.
  "
  [ast & settings] 
  (let [trace-atom 'guzheng.core/main-trace-atom
        transformed-ast (binding [*initialize-main-traces*
                                  (not (some #{:reload} settings))]
                          (walk-trace-branches ast trace-atom (ns-name *ns*)))
        new-ast transformed-ast]
    ;(clojure.pprint/pprint new-ast)
    ;(flush)
    new-ast))

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
        report-msgs (atom [])
        errors-so-far (atom #{})]
    (doseq [[id {:keys [type ast line ns parent] :as data}] results]
      (letfn [(report [msg stmt]
                (when-not (contains? @errors-so-far parent)
                  (swap! report-msgs conj
                         {:line line
                          :ns ns
                          :msg (str "in ns " ns ": "
                                    (print-str msg)
                                    " is not covered in \""
                                    stmt
                                    "\" on line " line)}))
                (swap! errors-so-far conj id)) 
              (report-fn [fn-decl]
                (let [[_ bodies] (extract-fn-arities ast)
                      arities (map #(vector (first %1) %2) 
                                   bodies
                                   (range))]
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
          :if-not (do
                    (when (zero? (:lhs data))
                      (report "true branch" "if-not"))
                    (when (zero? (:rhs data))
                      (report "false branch" "if-not")))
          :if-let (do
                    (when (zero? (:lhs data))
                      (report "true branch" "if-let"))
                    (when (zero? (:rhs data))
                      (report "false branch" "if-let")))
          :when (do
                  (when (zero? (:lhs data))
                    (report "body" "when")))
          :when-let (do
                  (when (zero? (:lhs data))
                    (report "body" "when-let")))
          :when-not (do
                      (when (zero? (:rhs data))
                        (report "body" "when-not")))
          :defn (report-fn "defn") 
          :defn- (report-fn "defn-") 
          :fn (report-fn "fn")
          :case (let [ast (drop 1 ast)
                      clauses (keep-indexed
                                vector
                                (partition 2 ast))
                      final (if (odd? (count ast))
                              [(-> (count ast)
                                 dec
                                 (/ 2))
                               (vector "default case" 
                                       (last ast))] 
                              nil)
                      clauses (if-not (nil? final)
                                (conj clauses final)
                                clauses)]
                  (doseq [[id clause] clauses]
                    (when (zero? (get data id))
                      (report (first clause) "case"))))
          :delay (when (zero? (:delayed data))
                   (report "body" "delay"))
          :defmethod (let [[multifn dispatch-val & fn-tail] ast
                           fn-tails (map vector
                                         (normalize-fntail fn-tail)
                                         (range))]
                       (doseq [[[args & _] id] fn-tails]
                         (when (zero? (get data id))
                           (report (str "arity " args)
                                   (str "defmethod for dispatch value "
                                        dispatch-val)))))
          :cond (let [clauses (keep-indexed
                                vector
                                (partition 2 ast))]
                  (doseq [[id clause] clauses]
                    (when (zero? (get data id))
                      (report (first clause) "cond"))))
          :condp (let [ast (drop 2 ast)
                       parsed-clauses (first (run 1 [q]
                                                  (match-clauses ast q)))
                       clauses (map #(assoc %1 :id %2)
                                    parsed-clauses (range))]
                   (doseq [{:keys [id test expr default]} clauses]
                     (when (zero? (get data id))
                       (report (if-not default
                                 test
                                 "last clause") "condp"))))
          nil)))
    (->> @report-msgs
      (sort-by (juxt :line :ns))
      (map :msg)
      (join "\n")
      println)))

(sleight/def-transform instrument
  :post report-missing-coverage
  :transform trace-if-branches)
