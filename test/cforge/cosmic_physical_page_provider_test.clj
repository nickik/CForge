(ns cforge.cosmic-physical-page-provider-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.cosmic-physical-page-provider :as phys]))

(deftest reserve-and-account
  (let [p (phys/make-provider 4096 16 {:base 0x10000000})]
    (is (= {:ok nil} (phys/reserve-range! p 0x10000000 8192)))
    (is (= {:total-pages 16 :reserved-pages 2 :allocated-pages 0 :free-pages 14}
           (phys/stats p)))
    (is (phys/valid-invariants? p))))

(deftest allocate-zero-free
  (let [p (phys/make-provider 4096 8)
        allocation (:ok (phys/alloc-pages! p 3 false))]
    (is allocation)
    (is (= 3 (:allocated-pages (phys/stats p))))
    (is (false? (phys/allocation-zeroed? p allocation)))
    (is (= {:ok nil} (phys/zero-pages! p allocation)))
    (is (phys/allocation-zeroed? p allocation))
    (is (= {:ok nil} (phys/free-pages! p allocation)))
    (is (= :not-allocated (:error (phys/free-pages! p allocation))))
    (is (= 8 (:free-pages (phys/stats p))))
    (is (phys/valid-invariants? p))))

(deftest reserved-pages-are-never-allocated
  (let [p (phys/make-provider 4096 6)]
    (is (= {:ok nil} (phys/reserve-pages! p 0 2)))
    (let [a (:ok (phys/alloc-pages! p 4 false))]
      (is (= #{2 3 4 5} (:pages a)))
      (is (= :out-of-memory (:error (phys/alloc-pages! p 1 false))))
      (is (phys/valid-invariants? p)))))

(deftest contiguous-allocation-can-fail-on-fragmentation
  (let [p (phys/make-provider 4096 6)]
    (is (= {:ok nil} (phys/reserve-pages! p 1 1)))
    (is (= {:ok nil} (phys/reserve-pages! p 3 1)))
    (is (= {:ok nil} (phys/reserve-pages! p 5 1)))
    (is (= :unsuitable-memory (:error (phys/alloc-pages! p 2 true))))
    (is (= 3 (:free-pages (phys/stats p))))
    (is (phys/valid-invariants? p))))

(deftest failed-allocation-does-not-consume-pages
  (let [p (phys/make-provider 4096 4 {:fail-after-allocations 0})]
    (is (= :out-of-memory (:error (phys/alloc-pages! p 1 false))))
    (is (= {:total-pages 4 :reserved-pages 0 :allocated-pages 0 :free-pages 4}
           (phys/stats p)))
    (is (phys/valid-invariants? p))))
