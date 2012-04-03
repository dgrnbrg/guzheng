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
