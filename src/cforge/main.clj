(ns cforge.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [cforge.core :as core]
            [cforge.conformance :as conformance]
            [cforge.host-services :as services]
            [cforge.lexer :as lexer]
            [cforge.trace :as trace]))

(defn- usage []
  (str "Usage:\n"
       "  cforge --tokens [--pprint] [--trace] FILE\n"
       "  cforge --ast [--pprint] [--trace] [--platform NAME] [--library NAME=PATH]... FILE\n"
       "  cforge --check [--pprint] [--trace] [--platform NAME] [--library NAME=PATH]... FILE\n"
       "  cforge --run [--pprint] [--trace] [--platform NAME] [--library NAME=PATH]... FILE [-- ARGS...]\n"
       "  cforge --conformance [--pprint] [--trace] SUITE.FDN\n"))

(defn- parse-library [spec]
  (let [[name path] (str/split spec #"=" 2)]
    (when (or (str/blank? name) (str/blank? path))
      (throw (ex-info "--library expects NAME=PATH" {:usage true})))
    [name path]))

(defn- parse-args [args]
  (loop [args (seq args)
         mode nil
         pretty? false
         trace? false
         platform nil
         libraries []
         program-args []
         positional []]
    (if-let [arg (first args)]
      (cond
        (= arg "--")
        (if (and (= mode :run) (= 1 (count positional)))
          (recur nil mode pretty? trace? platform libraries (vec (next args)) positional)
          (recur (next args) mode pretty? trace? platform libraries program-args positional))

        (contains? #{"--tokens" "--ast" "--check" "--run" "--conformance"} arg)
        (if mode
          (throw (ex-info "exactly one mode is required" {:usage true}))
          (recur (next args) (keyword (subs arg 2)) pretty? trace? platform libraries program-args positional))

        (= arg "--pprint")
        (recur (next args) mode true trace? platform libraries program-args positional)

        (= arg "--trace")
        (recur (next args) mode pretty? true platform libraries program-args positional)

        (= arg "--platform")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--platform requires a value" {:usage true})))
          (recur (nnext args) mode pretty? trace? value libraries program-args positional))

        (= arg "--library")
        (let [spec (second args)]
          (when-not spec
            (throw (ex-info "--library requires NAME=PATH" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform
                 (conj libraries (parse-library spec)) program-args positional))

        (str/starts-with? arg "--library=")
        (recur (next args) mode pretty? trace? platform
               (conj libraries (parse-library (subs arg (count "--library="))))
               program-args positional)

        (= arg "--program-arg")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--program-arg requires a value" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform libraries
                 (conj program-args value) positional))

        (str/starts-with? arg "--")
        (throw (ex-info (str "unknown option " arg) {:usage true}))

        :else
        (recur (next args) mode pretty? trace? platform libraries program-args (conj positional arg)))
      (do
        (when-not mode
          (throw (ex-info "exactly one mode is required" {:usage true})))
        (when (not= 1 (count positional))
          (throw (ex-info "exactly one input path is required" {:usage true})))
        {:mode mode
         :path (first positional)
         :pretty? pretty?
         :trace? trace?
         :platform platform
         :libraries libraries
         :program-args program-args}))))

(defn- print-data [x pretty?]
  (if pretty? (pprint/pprint x) (prn x)))

(defn- run-command [{:keys [mode path pretty? libraries program-args platform]}]
  ;; Platform is a build/provider selection, not Forge language semantics.
  ;; Bootstrap CForge records it as a JVM property so replaceable host service
  ;; providers can select implementations without changing Cosmic source.
  (when platform
    (System/setProperty "forge.platform" platform))
  (case mode
    :tokens
    (let [r (lexer/lex (slurp path))]
      (print-data r pretty?)
      (if (seq (:diagnostics r)) 1 0))

    :ast
    (let [r (core/parse-source (slurp path))]
      (print-data (select-keys r [:ast :diagnostics :phase]) pretty?)
      (if (seq (:diagnostics r)) 1 0))

    :check
    (let [r (core/check-source (slurp path) libraries)]
      (print-data (select-keys r [:typed-ast :diagnostics :phase]) pretty?)
      (if (seq (:diagnostics r)) 1 0))

    :run
    (binding [services/*program-args* program-args]
      (let [r (core/run-source (slurp path) libraries)]
        (if (seq (:diagnostics r))
          (do (print-data (select-keys r [:diagnostics :phase]) pretty?) 1)
          (do
            (when pretty? (print-data (select-keys r [:value :exit]) true))
            (:exit r)))))

    :conformance
    (let [r (conformance/run-suite path)]
      (print-data r pretty?)
      (if (:conforming? r) 0 1))))

(defn -main [& args]
  (try
    (let [{:keys [trace? pretty?] :as options} (parse-args args)
          sink (when trace? (trace/stderr-sink pretty?))]
      (binding [trace/*trace-sink* sink]
        (System/exit (int (run-command options)))))
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (.getMessage e))
        (when (:usage (ex-data e)) (print (usage))))
      (System/exit 2))
    (catch Throwable t
      (binding [*out* *err*]
        (println "CForge internal error:" (.getMessage t)))
      (System/exit 70))))
