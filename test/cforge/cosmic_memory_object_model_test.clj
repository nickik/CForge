(ns cforge.cosmic-memory-object-model-test
  (:require [clojure.test :refer [deftest is]]
            [cforge.cosmic-memory-pool-model :as pool]
            [cforge.cosmic-memory-object-model :as mem]
            [cforge.cosmic-physical-page-provider :as phys]))

(defn setup
  ([] (setup {}))
  ([store-options]
   {:pool (pool/make-root-pool (* 16 4096))
    :provider (phys/make-provider 4096 16)
    :store (mem/make-store store-options)}))

(deftest create-rounds-backing-and-publishes-handle
  (let [{:keys [pool provider store]} (setup {:metadata-charge 64})
        object (:ok (mem/create! store pool provider {:size 5000
                                                      :physically-contiguous? false}))]
    (is object)
    (is (= 5000 (:requested-size object)))
    (is (= 8192 (:backing-size object)))
    (is (= 2 (:page-count object)))
    (is (= 8256 (:total-charge object)))
    (is (= 8256 (:charged (pool/usage pool))))
    (is (= 2 (:allocated-pages (phys/stats provider))))
    (is (phys/allocation-zeroed? provider (:allocation object)))
    (is (= 1 (mem/object-count store)))
    (is (= 1 (mem/kernel-object-count store)))
    (is (= 1 (mem/live-handle-count store)))
    (is (= 0 (mem/reserved-handle-count store)))
    (is (= {:ok (:id object)} (mem/lookup-handle store (:handle object))))
    (is (pool/valid-invariants? pool))
    (is (phys/valid-invariants? provider))))

(deftest destruction-releases-handle-object-backing-and-accounting
  (let [{:keys [pool provider store]} (setup)
        object (:ok (mem/create! store pool provider {:size 4096
                                                      :physically-contiguous? false}))
        old-handle (:handle object)]
    (is (= {:ok nil} (mem/destroy! store pool provider object)))
    (is (= 0 (:charged (pool/usage pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))
    (is (= :invalid-handle (:error (mem/lookup-handle store old-handle))))
    (is (= :invalid-object (:error (mem/destroy! store pool provider object))))))

(deftest pool-limit-failure-does-not-touch-provider
  (let [memory-pool (pool/make-root-pool 4096)
        provider (phys/make-provider 4096 8)
        store (mem/make-store {:metadata-charge 64})]
    (is (= :limit-exceeded
           (:error (mem/create! store memory-pool provider {:size 4096
                                                            :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage memory-pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))))

(deftest provider-failure-rolls-back-charge
  (let [memory-pool (pool/make-root-pool 65536)
        provider (phys/make-provider 4096 8 {:fail-after-allocations 0})
        store (mem/make-store)]
    (is (= :out-of-memory
           (:error (mem/create! store memory-pool provider {:size 4096
                                                            :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage memory-pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))))

(deftest failure-after-charge-rolls-back-everything
  (let [{:keys [pool provider]} (setup)
        store (mem/make-store {:fail-stage :after-charge})]
    (is (= :injected-failure
           (:error (mem/create! store pool provider {:size 4096
                                                     :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))))

(deftest failure-after-physical-allocation-rolls-back-everything
  (let [{:keys [pool provider]} (setup)
        store (mem/make-store {:fail-stage :after-physical-allocation})]
    (is (= :injected-failure
           (:error (mem/create! store pool provider {:size 4096
                                                     :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))))

(deftest failure-after-object-construction-rolls-back-everything
  (let [{:keys [pool provider]} (setup)
        store (mem/make-store {:fail-stage :after-object-construction})]
    (is (= :injected-failure
           (:error (mem/create! store pool provider {:size 4096
                                                     :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))))

(deftest failure-before-publish-cancels-reservation-and-rolls-back
  (let [{:keys [pool provider]} (setup)
        store (mem/make-store {:fail-stage :before-publish})]
    (is (= :injected-failure
           (:error (mem/create! store pool provider {:size 4096
                                                     :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))
    (is (= 0 (mem/reserved-handle-count store)))
    (is (pool/valid-invariants? pool))
    (is (phys/valid-invariants? provider))))

(deftest failure-after-publish-unpublishes-and-rolls-back
  (let [{:keys [pool provider]} (setup)
        store (mem/make-store {:fail-stage :after-publish})]
    (is (= :injected-failure
           (:error (mem/create! store pool provider {:size 4096
                                                     :physically-contiguous? false}))))
    (is (= 0 (:charged (pool/usage pool))))
    (is (= 0 (:allocated-pages (phys/stats provider))))
    (is (= 0 (mem/object-count store)))
    (is (= 0 (mem/kernel-object-count store)))
    (is (= 0 (mem/live-handle-count store)))
    (is (= 0 (mem/reserved-handle-count store)))))

(deftest handle-table-full-rolls-back-object-pages-and-charge
  (let [{:keys [pool provider]} (setup)
        store (mem/make-store {:handle-capacity 1})
        first-object (:ok (mem/create! store pool provider {:size 4096
                                                            :physically-contiguous? false}))]
    (is first-object)
    (let [before-charge (:charged (pool/usage pool))
          before-pages (:allocated-pages (phys/stats provider))]
      (is (= :publish-failure
             (:error (mem/create! store pool provider {:size 4096
                                                       :physically-contiguous? false}))))
      (is (= before-charge (:charged (pool/usage pool))))
      (is (= before-pages (:allocated-pages (phys/stats provider))))
      (is (= 1 (mem/object-count store)))
      (is (= 1 (mem/kernel-object-count store)))
      (is (= 1 (mem/live-handle-count store)))
      (is (= 0 (mem/reserved-handle-count store))))))
