(ns cforge.conformance
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cforge.core :as core]
            [cforge.trace :as trace]))

(def unsupported-categories
  #{:parse/unsupported-syntax :type/unsupported :runtime/unsupported})

(defn- read-suite [suite-file]
  (edn/read-string {:readers {'path identity}}
                   (slurp suite-file)))

(defn- source-file [suite-file relative]
  (io/file (.getParentFile (io/file suite-file)) relative))

(defn- first-category [result]
  (some-> result :diagnostics first :category))

(defn- classify-result [test result]
  (let [kind (:kind test)
        diagnostics (:diagnostics result)
        category (first-category result)]
    (case kind
      :parse
      (cond
        (empty? diagnostics) {:status :pass}
        (contains? unsupported-categories category)
        {:status :unsupported :reason category}
        :else {:status :fail :reason :unexpected-diagnostic :actual category})

      :negative
      (let [expected (:expect test)]
        (cond
          (empty? diagnostics) {:status :fail :reason :expected-rejection}
          (= expected category) {:status :pass}
          (contains? unsupported-categories category)
          {:status :unsupported :reason category}
          :else {:status :fail :reason :wrong-diagnostic
                 :expected expected :actual category}))

      :run
      (cond
        (seq diagnostics)
        {:status (if (contains? unsupported-categories category) :unsupported :fail)
         :reason category}

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
       :conforming? (and (zero? (get counts :fail 0))
                         (zero? (get counts :unsupported 0)))
       :results results})))
