(ns cforge.spec-coverage-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [cforge.core :as core]))

(def coverage-file "test/fixtures/forge_v1/coverage.edn")

(defn suite [] (edn/read-string (slurp coverage-file)))

(def required-feature-families
  #{"lex" "literal" "module" "binding" "type" "conversion" "arithmetic"
    "logic" "flow" "pattern" "function" "closure" "result" "safety"
    "context" "reader" "metadata" "impl" "concurrency" "ffi" "absent"})

(defn feature-family [feature]
  (namespace feature))

(defn execute-case [{:keys [phase source]}]
  ((case phase
     :parse core/parse-source
     :check core/check-source
     :run core/run-source)
   source))

(deftest coverage-corpus-is-large-and-well-formed
  (let [cases (:cases (suite))
        ids (map :id cases)]
    (is (>= (count cases) 70) "Forge v1 corpus should remain broad")
    (is (= (count ids) (count (distinct ids))) "coverage case IDs must be unique")
    (doseq [c cases]
      (is (keyword? (:id c)) (pr-str c))
      (is (keyword? (:feature c)) (pr-str c))
      (is (contains? #{:parse :check :run} (:phase c)) (pr-str c))
      (is (contains? #{:accept :reject :exit} (:expect c)) (pr-str c))
      (is (string? (:source c)) (pr-str c)))
    (let [families (set (map (comp feature-family :feature) cases))]
      (doseq [family required-feature-families]
        (is (contains? families family)
            (str "missing Forge v1 feature family " family))))))

(deftest future-cases-are-explicitly-marked
  (doseq [c (:cases (suite))
          :when (not (:bootstrap c))]
    (is (= :future (:status c))
        (str "non-bootstrap case must be explicitly future: " (:id c)))))

(deftest bootstrap-corpus-matches-current-reference-semantics
  (doseq [c (:cases (suite))
          :when (:bootstrap c)]
    (testing (name (:id c))
      (let [result (execute-case c)
            diagnostics (:diagnostics result)
            actual-category (get-in result [:diagnostics 0 :category])]
        (case (:expect c)
          :accept
          (is (empty? diagnostics) (pr-str result))

          :exit
          (do
            (is (empty? diagnostics) (pr-str result))
            (is (= (:exit c) (:exit result)) (pr-str result)))

          :reject
          (do
            (is (seq diagnostics) (pr-str result))
            (is (= (:category c) actual-category) (pr-str result))))))))
