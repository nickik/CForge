(ns cforge.sia-machine-conformance-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.sia-machine :as sia]))

(deftest initialization-is-harness-state-not-an-instruction
  (sia/initialize-machine! 8 1048618)
  (let [observation (sia/observation)]
    (is (= sia/observation-schema (:schema observation)))
    (is (= 8N (:status observation)))
    (is (= 1048618N (:vmctx observation)))
    (is (empty? (:events observation)))))

(deftest observation-captures-ordered-architectural-effects
  ;; Root 0x00100000 with ASID 42.
  (sia/initialize-machine! 0 1048618)
  (sia/status-write! 31)
  (sia/vmctx-write! 2097223) ; root 0x00200000, ASID 71
  (sia/tlb-fence-va! 65536)
  (sia/tlb-fence-asid! 9)
  (sia/tlb-fence-all!)

  (let [{:keys [status vmctx events]} (sia/observation)]
    (is (= 15N status))
    (is (= 2097223N vmctx))
    (is (= [{:seq 0N :op :status-write :requested 31N :stored 15N}
            {:seq 1N :op :vmctx-write :value 2097223N}
            {:seq 2N :op :tlb-fence-va :va 65536N :asid 71N}
            {:seq 3N :op :tlb-fence-asid :asid 9N}
            {:seq 4N :op :tlb-fence-all}]
           events)))

  (is (= (str "sia32-machine-observation-v1\n"
              "status\t15\n"
              "vmctx\t2097223\n"
              "event\t0\tstatus_write\t31\t15\n"
              "event\t1\tvmctx_write\t2097223\n"
              "event\t2\ttlb_fence_va\t65536\t71\n"
              "event\t3\ttlb_fence_asid\t9\n"
              "event\t4\ttlb_fence_all\n")
         (sia/observation-text))))

(deftest hosted-machine-rejects-values-native-sia32-cannot-represent
  (testing "architectural words stay 32-bit"
    (is (thrown? clojure.lang.ExceptionInfo (sia/initialize-machine! 0 4294967296N)))
    (is (thrown? clojure.lang.ExceptionInfo (sia/vmctx-write! 4294967296N)))
    (is (thrown? clojure.lang.ExceptionInfo (sia/tlb-fence-va! 4294967296N))))
  (testing "ASID operands stay 12-bit"
    (is (thrown? clojure.lang.ExceptionInfo (sia/tlb-fence-asid! 4096)))))
