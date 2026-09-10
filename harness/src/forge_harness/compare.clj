(ns forge-harness.compare)

(defn diagnostic-categories [result]
  (mapv :category (:diagnostics result)))

(defn signature
  "Return the normative comparison signature for one normalized adapter result."
  [level result]
  (case level
    :acceptance
    (select-keys result [:outcome :phase])

    :diagnostics
    (assoc (select-keys result [:outcome :phase])
           :diagnostic-categories (diagnostic-categories result))

    :execution
    (assoc (select-keys result [:outcome :phase :program-exit
                                :program-stdout :program-stderr])
           :diagnostic-categories (diagnostic-categories result))

    :ast
    (assoc (select-keys result [:outcome :phase :canonical-ast-version :canonical-ast])
           :diagnostic-categories (diagnostic-categories result))

    (throw (ex-info (str "unknown comparison level: " level)
                    {:category :harness/invalid-suite}))))

(defn compare-results [level implementation-results]
  (let [signatures (into {}
                         (map (fn [[id result]] [id (signature level result)]))
                         implementation-results)
        values (vals signatures)]
    {:match? (or (empty? values) (apply = values))
     :level level
     :signatures signatures}))
