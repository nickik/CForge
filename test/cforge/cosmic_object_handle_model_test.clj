(ns cforge.cosmic-object-handle-model-test
  (:require [clojure.test :refer [deftest is]]
            [cforge.cosmic-object-handle-model :as oh]))

(deftest constructing-object-is-not-a-handle
  (let [objects (oh/make-object-table)
        handles (oh/make-handle-table 2)
        object (:ok (oh/create-object! objects :memory-object {:size 4096}))]
    (is (= :constructing (:state object)))
    (is (= 1 (oh/object-count objects)))
    (is (= 0 (oh/live-handle-count handles)))))

(deftest reserve-publish-lookup
  (let [objects (oh/make-object-table)
        handles (oh/make-handle-table 2)
        object (:ok (oh/create-object! objects :memory-object {}))
        reservation (:ok (oh/reserve-slot! handles))
        published (:ok (oh/publish! handles objects reservation (:id object)))]
    (is (= {:ok (:id object)} (oh/lookup handles (:handle published))))
    (is (= :live (:state (oh/object objects (:id object)))))
    (is (= 1 (oh/live-handle-count handles)))
    (is (= 0 (oh/reserved-handle-count handles)))))

(deftest publication-validates-object-identity-and-state
  (let [objects (oh/make-object-table)
        handles (oh/make-handle-table 2)
        reservation-a (:ok (oh/reserve-slot! handles))]
    (is (= :invalid-object
           (:error (oh/publish! handles objects reservation-a 999))))
    (is (= {:ok nil} (oh/cancel-reservation! handles reservation-a)))
    (let [object (:ok (oh/create-object! objects :memory-object {}))
          _ (oh/mark-live! objects (:id object))
          reservation-b (:ok (oh/reserve-slot! handles))]
      (is (= :wrong-object-state
             (:error (oh/publish! handles objects reservation-b (:id object)))))
      (is (= {:ok nil} (oh/cancel-reservation! handles reservation-b))))))

(deftest cancellation-never-publishes
  (let [handles (oh/make-handle-table 1)
        reservation (:ok (oh/reserve-slot! handles))]
    (is (= 1 (oh/reserved-handle-count handles)))
    (is (= {:ok nil} (oh/cancel-reservation! handles reservation)))
    (is (= 0 (oh/reserved-handle-count handles)))
    (is (= 0 (oh/live-handle-count handles)))))

(deftest stale-handle-rejected-after-unpublish
  (let [objects (oh/make-object-table)
        handles (oh/make-handle-table 1)
        object (:ok (oh/create-object! objects :memory-object {}))
        reservation (:ok (oh/reserve-slot! handles))
        handle (:handle (:ok (oh/publish! handles objects reservation (:id object))))]
    (is (= {:ok nil} (oh/unpublish! handles handle)))
    (is (= :invalid-handle (:error (oh/lookup handles handle))))
    (let [new-reservation (:ok (oh/reserve-slot! handles))]
      (is (> (:generation new-reservation) (:generation handle))))))

(deftest table-full-is-explicit
  (let [objects (oh/make-object-table)
        handles (oh/make-handle-table 1)
        object (:ok (oh/create-object! objects :memory-object {}))
        reservation (:ok (oh/reserve-slot! handles))]
    (is (:ok (oh/publish! handles objects reservation (:id object))))
    (is (= :table-full (:error (oh/reserve-slot! handles))))))
