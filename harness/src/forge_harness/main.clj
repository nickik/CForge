(ns forge-harness.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [forge-harness.compare :as compare]
            [forge-harness.process :as process]))

(defn- read-edn-file [path]
  (edn/read-string (slurp path)))

(defn- enabled-implementations [config]
  (filterv :enabled (:implementations config)))

(defn- resolve-path [base relative]
  (.getCanonicalFile (io/file base relative)))

(defn- operation-for [test]
  (case (:kind test)
    :parse :parse
    :negative :check
    :run :run
    (throw (ex-info (str "unknown test kind: " (:kind test))
                    {:category :harness/invalid-suite}))))

(defn- default-level [test]
  (case (:kind test)
    :parse :acceptance
    :negative :diagnostics
    :run :execution))

(defn- expected-result? [test result]
  (case (:kind test)
    :parse
    (= :accepted (:outcome result))

    :negative
    (and (= :rejected (:outcome result))
         (or (nil? (:expect test))
             (= (:expect test) (some-> result :diagnostics first :category))))

    :run
    (and (= :accepted (:outcome result))
         (= (:exit test) (:program-exit result))
         (or (not (contains? test :stdout))
             (= (:stdout test) (:program-stdout result)))
         (or (not (contains? test :stderr))
             (= (:stderr test) (:program-stderr result))))

    false))

(defn- run-one-implementation [suite-dir impl test]
  (let [file (resolve-path suite-dir (:path test))
        invocation (process/invoke (:command impl) (operation-for test) file)]
    [(:id impl) invocation]))

(defn- run-test [suite-dir implementations test]
  (let [invocations (into {}
                          (map #(run-one-implementation suite-dir % test))
                          implementations)
        infra-failures (into {}
                             (filter (fn [[_ x]] (not= :ok (:status x))))
                             invocations)
        normalized (into {}
                         (keep (fn [[id x]]
                                 (when (= :ok (:status x)) [id (:result x)])))
                         invocations)
        expectations (into {}
                           (map (fn [[id result]] [id (expected-result? test result)]))
                           normalized)
        level (or (:compare test) (default-level test))
        comparison (when (empty? infra-failures)
                     (compare/compare-results level normalized))]
    {:id (:id test)
     :path (:path test)
     :kind (:kind test)
     :compare level
     :pass? (and (empty? infra-failures)
                 (:match? comparison)
                 (every? true? (vals expectations))
                 (not-any? #(= :unsupported (:outcome %)) (vals normalized)))
     :expectations expectations
     :comparison comparison
     :infrastructure-failures infra-failures
     :results normalized
     :raw-invocations invocations}))

(defn run-suite [config-path suite-path]
  (let [config (read-edn-file config-path)
        suite (read-edn-file suite-path)
        implementations (enabled-implementations config)
        suite-dir (.getParentFile (.getCanonicalFile (io/file suite-path)))]
    (when (empty? implementations)
      (throw (ex-info "no implementations enabled" {:category :harness/invalid-config})))
    (let [results (mapv #(run-test suite-dir implementations %) (:tests suite))]
      {:harness-protocol 1
       :suite (:suite suite)
       :implementations (mapv :id implementations)
       :tests (count results)
       :passed (count (filter :pass? results))
       :failed (count (remove :pass? results))
       :conforming? (every? :pass? results)
       :results results})))

(defn -main [& args]
  (let [[flag config suite & extra] args]
    (if (or (not= flag "--config") (nil? config) (nil? suite) (seq extra))
      (do
        (binding [*out* *err*]
          (println "usage: clojure -M -m forge-harness.main --config CONFIG.EDN SUITE.EDN"))
        (System/exit 2))
      (try
        (let [result (run-suite config suite)]
          (pprint/pprint result)
          (System/exit (if (:conforming? result) 0 1)))
        (catch Throwable t
          (binding [*out* *err*]
            (pprint/pprint {:category :harness/internal-error
                            :message (.getMessage t)
                            :data (ex-data t)}))
          (System/exit 70))))))
