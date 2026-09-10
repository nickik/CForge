(ns cforge.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [cforge.core :as core]
            [cforge.conformance :as conformance]
            [cforge.lexer :as lexer]
            [cforge.trace :as trace]))

(defn- usage []
  (str "Usage:\n"
       "  cforge --tokens [--pprint] [--trace] FILE\n"
       "  cforge --ast [--pprint] [--trace] FILE\n"
       "  cforge --check [--pprint] [--trace] FILE\n"
       "  cforge --run [--pprint] [--trace] FILE\n"
       "  cforge --conformance [--pprint] [--trace] SUITE.FDN\n"))

(defn- parse-args [args]
  (let [flags (set (filter #(str/starts-with? % "--") args))
        positional (vec (remove #(str/starts-with? % "--") args))
        modes (filter flags ["--tokens" "--ast" "--check" "--run" "--conformance"])]
    (when (not= 1 (count modes))
      (throw (ex-info "exactly one mode is required" {:usage true})))
    (when (not= 1 (count positional))
      (throw (ex-info "exactly one input path is required" {:usage true})))
    {:mode (keyword (subs (first modes) 2))
     :path (first positional)
     :pretty? (contains? flags "--pprint")
     :trace? (contains? flags "--trace")}))

(defn- print-data [x pretty?]
  (if pretty? (pprint/pprint x) (prn x)))

(defn- run-command [{:keys [mode path pretty?]}]
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
    (let [r (core/check-source (slurp path))]
      (print-data (select-keys r [:typed-ast :diagnostics :phase]) pretty?)
      (if (seq (:diagnostics r)) 1 0))

    :run
    (let [r (core/run-source (slurp path))]
      (if (seq (:diagnostics r))
        (do (print-data (select-keys r [:diagnostics :phase]) pretty?) 1)
        (do
          (when pretty? (print-data (select-keys r [:value :exit]) true))
          (:exit r))))

    :conformance
    (let [r (conformance/run-suite path)]
      (print-data r pretty?)
      (if (zero? (get-in r [:counts :fail])) 0 1))))

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
