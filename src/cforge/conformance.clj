(ns cforge.conformance
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cforge.core :as core]
            [cforge.trace :as trace]))

(defn- read-suite [suite-file]
  (edn/read-string {:readers {'path identity}}
                   (slurp suite-file)))

(defn- source-file [suite-file relative]
  (io/file (.getParentFile (io/file suite-file)) relative))

(defn- classify-result [test result]
  (let [kind (:kind test)
        diagnostics (:diagnostics result)]
    (case kind
      :parse
      (if (seq diagnostics)
        {:status :fail :reason :unexpected-diagnostic}
        {:status :pass})

      :negative
      (let [expected (:expect test)
            actual (some-> diagnostics first :category)]
        (cond
          (empty? diagnostics) {:status :fail :reason :expected-rejection}
          (= expected actual) {:status :pass}
          (= :parse/unsupported-syntax actual) {:status :unsupported :reason actual}
          (= :type/unsupported actual) {:status :unsupported :reason actual}
          :else {:status :fail :reason :wrong-diagnostic :expected expected :actual actual}))

      :run
      (cond
        (seq diagnostics)
        (let [category (some-> diagnostics first :category)]
          {:status (if (contains? #{:parse/unsupported-syntax :type/unsupported :runtime/unsupported}
                                   category)
                     :unsupported
                     :fail)
           :reason category})

        (= (:exit test) (:exit result)) {:status :pass}
        :else {:status :fail :reason :wrong-exit
               :expected (:exit test) :actual (:exit result)})

      {:status :fail :reason :unknown-test-kind})))

(defn run-test [suite-file test]
  (let [path (:path test)
        file (source-file suite-file path)
        source (slurp file)
        result (case (:kind test)
                 :parse (core/parse-source source)
                 :negative (core/check-source source)
                 :run (core/run-source source))
        verdict (classify-result test result)
        out (merge {:kind (:kind test)
                    :path path
                    :phase (:phase result)
                    :diagnostics (:diagnostics result)
                    :exit (:exit result)}
                   verdict)]
    (trace/emit! {:event :conformance/test :path path :status (:status out)})
    out))

(defn run-suite [suite-file]
  (trace/with-phase :conformance
    (let [suite (read-suite suite-file)
          results (mapv #(run-test suite-file %) (:tests suite))
          counts (frequencies (map :status results))]
      {:suite (:suite suite)
       :version (:version suite)
       :counts {:pass (get counts :pass 0)
                :fail (get counts :fail 0)
                :unsupported (get counts :unsupported 0)}
       :results results})))
