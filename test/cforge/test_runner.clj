(ns cforge.test-runner
  (:require [clojure.test :as t]
            [cforge.application-test]
            [cforge.arena-provider-test]
            [cforge.ckv-test]
            [cforge.core-test]
            [cforge.cosmic-memory-pool-model-test]
            [cforge.cosmic-physical-page-provider-test]
            [cforge.cosmic-object-handle-model-test]
            [cforge.cosmic-memory-object-model-test]
            [cforge.freestanding-libraries-test]
            [cforge.host-services-test]
            [cforge.hosted-std-forge-test]
            [cforge.library-contract-test]
            [cforge.module-link-test]
            [cforge.spec-coverage-test]
            [cforge.transducer-contract-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'cforge.application-test
                                          'cforge.arena-provider-test
                                          'cforge.ckv-test
                                          'cforge.core-test
                                          'cforge.cosmic-memory-pool-model-test
                                          'cforge.cosmic-physical-page-provider-test
                                          'cforge.cosmic-object-handle-model-test
                                          'cforge.cosmic-memory-object-model-test
                                          'cforge.freestanding-libraries-test
                                          'cforge.host-services-test
                                          'cforge.hosted-std-forge-test
                                          'cforge.library-contract-test
                                          'cforge.module-link-test
                                          'cforge.spec-coverage-test
                                          'cforge.transducer-contract-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
