(ns cforge.test-runner
  (:require [clojure.test :as t]
            [cforge.application-test]
            [cforge.arena-provider-test]
            [cforge.core-test]
            [cforge.freestanding-libraries-test]
            [cforge.library-contract-test]
            [cforge.spec-coverage-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'cforge.application-test
                                          'cforge.arena-provider-test
                                          'cforge.core-test
                                          'cforge.freestanding-libraries-test
                                          'cforge.library-contract-test
                                          'cforge.spec-coverage-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
