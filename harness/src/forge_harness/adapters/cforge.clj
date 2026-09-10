(ns forge-harness.adapters.cforge
  (:require [cforge.core :as core]))

(def unsupported-categories
  #{:parse/unsupported-syntax :type/unsupported :runtime/unsupported})

(defn- coarse-phase [phase]
  (case phase
    :eval :run
    :lex :lex
    :parse :parse
    :resolve :resolve
    :check :check
    :run :run
    phase))

(defn- outcome [result]
  (let [diagnostics (:diagnostics result)
        categories (set (map :category diagnostics))]
    (cond
      (empty? diagnostics) :accepted
      (some unsupported-categories categories) :unsupported
      :else :rejected)))

(defn- base-result [operation result]
  {:protocol 1
   :implementation :cforge
   :operation operation
   :outcome (outcome result)
   :phase (coarse-phase (:phase result))
   :diagnostics (vec (:diagnostics result))})

(defn- parse-result [source]
  (let [result (core/parse-source source)]
    (cond-> (base-result :parse result)
      (:ast result) (assoc :raw-ast (:ast result)))))

(defn- check-result [source]
  (let [result (core/check-source source)]
    (cond-> (base-result :check result)
      (:typed-ast result) (assoc :raw-ast (:typed-ast result)))))

(defn- run-result [source]
  ;; CForge currently has no user-visible I/O runtime in the bootstrap subset,
  ;; so program stdout/stderr are explicitly empty rather than inferred.
  (let [result (core/run-source source)]
    (cond-> (assoc (base-result :run result)
                   :program-stdout ""
                   :program-stderr "")
      (and (= :accepted (outcome result)) (contains? result :exit))
      (assoc :program-exit (:exit result)))))

(defn- describe []
  {:protocol 1
   :implementation :cforge
   :implementation-version "bootstrap"
   :capabilities #{:parse :check :run}
   :canonical-ast-version nil})

(defn -main [& args]
  (try
    (let [[operation path & extra] args]
      (when (or (nil? operation) (seq extra))
        (throw (ex-info "usage: ADAPTER describe | parse FILE | check FILE | run FILE"
                        {:category :harness/usage})))
      (let [result
            (case operation
              "describe" (do
                           (when path
                             (throw (ex-info "describe takes no file" {:category :harness/usage})))
                           (describe))
              "parse" (parse-result (slurp path))
              "check" (check-result (slurp path))
              "run" (run-result (slurp path))
              (throw (ex-info (str "unknown operation: " operation)
                              {:category :harness/usage})))]
        (prn result)))
    (catch Throwable t
      (binding [*out* *err*]
        (prn {:category :harness/adapter-failure
              :message (.getMessage t)}))
      (System/exit 70))))
