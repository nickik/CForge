(ns cforge.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [cforge.core :as core]
            [cforge.conformance :as conformance]
            [cforge.host-services :as services]
            [cforge.lexer :as lexer]
            [cforge.sia-machine :as sia]
            [cforge.trace :as trace]))

(defn- usage []
  (str "Usage:\n"
       "  cforge --tokens [--pprint] [--trace] FILE\n"
       "  cforge --ast [--pprint] [--trace] [--platform NAME] [--library NAME=PATH]... FILE\n"
       "  cforge --check [--pprint] [--trace] [--platform NAME] [--library NAME=PATH]... FILE\n"
       "  cforge --run [--pprint] [--trace] [--platform NAME] [--library NAME=PATH]...\n"
       "         [--sia-status DECIMAL] [--sia-vmctx DECIMAL] [--sia-observation PATH]\n"
       "         FILE [-- ARGS...]\n"
       "  cforge --conformance [--pprint] [--trace] SUITE.FDN\n"))

(defn- parse-library [spec]
  (let [[name path] (str/split spec #"=" 2)]
    (when (or (str/blank? name) (str/blank? path))
      (throw (ex-info "--library expects NAME=PATH" {:usage true})))
    [name path]))

(defn- parse-decimal [option value]
  (try
    (bigint (java.math.BigInteger. ^String value))
    (catch Throwable _
      (throw (ex-info (str option " expects an unsigned decimal integer") {:usage true})))))

(defn- parse-args [args]
  (loop [args (seq args)
         mode nil
         pretty? false
         trace? false
         platform nil
         libraries []
         program-args []
         positional []
         sia-status 0N
         sia-vmctx 0N
         sia-observation nil]
    (if-let [arg (first args)]
      (cond
        (= arg "--")
        (if (and (= mode :run) (= 1 (count positional)))
          (recur nil mode pretty? trace? platform libraries (vec (next args)) positional
                 sia-status sia-vmctx sia-observation)
          (recur (next args) mode pretty? trace? platform libraries program-args positional
                 sia-status sia-vmctx sia-observation))

        (contains? #{"--tokens" "--ast" "--check" "--run" "--conformance"} arg)
        (if mode
          (throw (ex-info "exactly one mode is required" {:usage true}))
          (recur (next args) (keyword (subs arg 2)) pretty? trace? platform libraries program-args positional
                 sia-status sia-vmctx sia-observation))

        (= arg "--pprint")
        (recur (next args) mode true trace? platform libraries program-args positional
               sia-status sia-vmctx sia-observation)

        (= arg "--trace")
        (recur (next args) mode pretty? true platform libraries program-args positional
               sia-status sia-vmctx sia-observation)

        (= arg "--platform")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--platform requires a value" {:usage true})))
          (recur (nnext args) mode pretty? trace? value libraries program-args positional
                 sia-status sia-vmctx sia-observation))

        (= arg "--library")
        (let [spec (second args)]
          (when-not spec
            (throw (ex-info "--library requires NAME=PATH" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform
                 (conj libraries (parse-library spec)) program-args positional
                 sia-status sia-vmctx sia-observation))

        (str/starts-with? arg "--library=")
        (recur (next args) mode pretty? trace? platform
               (conj libraries (parse-library (subs arg (count "--library="))))
               program-args positional sia-status sia-vmctx sia-observation)

        (= arg "--program-arg")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--program-arg requires a value" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform libraries
                 (conj program-args value) positional sia-status sia-vmctx sia-observation))

        (= arg "--sia-status")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--sia-status requires a decimal value" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform libraries program-args positional
                 (parse-decimal "--sia-status" value) sia-vmctx sia-observation))

        (= arg "--sia-vmctx")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--sia-vmctx requires a decimal value" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform libraries program-args positional
                 sia-status (parse-decimal "--sia-vmctx" value) sia-observation))

        (= arg "--sia-observation")
        (let [value (second args)]
          (when-not value
            (throw (ex-info "--sia-observation requires a path" {:usage true})))
          (recur (nnext args) mode pretty? trace? platform libraries program-args positional
                 sia-status sia-vmctx value))

        (str/starts-with? arg "--")
        (throw (ex-info (str "unknown option " arg) {:usage true}))

        :else
        (recur (next args) mode pretty? trace? platform libraries program-args (conj positional arg)
               sia-status sia-vmctx sia-observation))
      (do
        (when-not mode
          (throw (ex-info "exactly one mode is required" {:usage true})))
        (when (not= 1 (count positional))
          (throw (ex-info "exactly one input path is required" {:usage true})))
        (when (and (not= mode :run)
                   (or (not= sia-status 0N) (not= sia-vmctx 0N) sia-observation))
          (throw (ex-info "SIA machine options are valid only with --run" {:usage true})))
        {:mode mode
         :path (first positional)
         :pretty? pretty?
         :trace? trace?
         :platform platform
         :libraries libraries
         :program-args program-args
         :sia-status sia-status
         :sia-vmctx sia-vmctx
         :sia-observation sia-observation}))))

(defn- print-data [x pretty?]
  (if pretty? (pprint/pprint x) (prn x)))

(defn- run-command [{:keys [mode path pretty? libraries program-args platform
                             sia-status sia-vmctx sia-observation]}]
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
      ;; Every CLI run begins from explicit SIA state. Initialization is harness
      ;; setup, not a simulated SIA instruction, and therefore emits no event.
      (sia/initialize-machine! sia-status sia-vmctx)
      (let [r (core/run-source (slurp path) libraries)]
        (when sia-observation
          (spit sia-observation (sia/observation-text)))
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
