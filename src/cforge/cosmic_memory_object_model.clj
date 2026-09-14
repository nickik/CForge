(ns cforge.cosmic-memory-object-model
  (:require [cforge.cosmic-memory-pool-model :as pool]
            [cforge.cosmic-object-handle-model :as oh]
            [cforge.cosmic-physical-page-provider :as phys]))

;; LEGACY SEMANTIC ORACLE ONLY.
;;
;; This namespace predates direct execution of the corresponding Cosmic Forge
;; kernel code. It must not be used as a host/platform implementation and must
;; not acquire new Cosmic policy. The live Cosmic path executes MemoryObject,
;; HandleSpace, AddressSpace, MMU transaction, backing lifetime and SIA32 page-
;; table logic in Forge. CForge/host-specific behavior belongs only below that
;; code in raw storage / physical memory / privileged machine effects.
;;
;; Keep this model only for historical differential vectors until those tests
;; are migrated to direct Forge execution, then remove it.

(defn round-up [value align]
  (* (quot (+ value (dec align)) align) align))

(defn make-store
  ([] (make-store {}))
  ([{:keys [metadata-charge fail-stage handle-capacity]
     :or {metadata-charge 64 fail-stage nil handle-capacity 64}}]
   {:state (atom {:metadata-charge metadata-charge
                  :fail-stage fail-stage
                  :objects {}})
    :object-table (oh/make-object-table)
    :handle-table (oh/make-handle-table handle-capacity)}))

(defn store-state [store] @(:state store))
(defn- fail? [store stage] (= stage (:fail-stage (store-state store))))

(defn- rollback! [store memory-pool provider charge allocation reservation object-id published-handle]
  (when published-handle (oh/unpublish! (:handle-table store) published-handle))
  (when object-id (oh/destroy-object! (:object-table store) object-id))
  (when reservation (oh/cancel-reservation! (:handle-table store) reservation))
  (when allocation (phys/free-pages! provider allocation))
  (when charge (pool/release! memory-pool charge))
  nil)

(defn- commit-record!
  [store request size backing-size page-count metadata-charge total-charge charge allocation object-id published]
  (let [live-object (oh/object (:object-table store) object-id)
        object-record {:id object-id
                       :header live-object
                       :handle (:handle published)
                       :requested-size size
                       :backing-size backing-size
                       :page-count page-count
                       :metadata-charge metadata-charge
                       :total-charge total-charge
                       :charge charge
                       :allocation allocation
                       :attributes (dissoc request :size)}]
    (swap! (:state store) assoc-in [:objects object-id] object-record)
    {:ok object-record}))

(defn create!
  [store memory-pool provider {:keys [size physically-contiguous?] :as request}]
  (if-not (and (integer? size) (pos? size))
    {:error :invalid-size}
    (let [{:keys [page-size]} (phys/geometry provider)
          metadata-charge (:metadata-charge (store-state store))
          backing-size (round-up size page-size)
          page-count (quot backing-size page-size)
          total-charge (+ backing-size metadata-charge)
          charge-result (pool/charge! memory-pool total-charge :memory-object)]
      (if-let [charge (:ok charge-result)]
        (if (fail? store :after-charge)
          (do (rollback! store memory-pool provider charge nil nil nil nil)
              {:error :injected-failure})
          (let [allocation-result (phys/alloc-pages! provider page-count (boolean physically-contiguous?))]
            (if-let [allocation (:ok allocation-result)]
              (if (fail? store :after-physical-allocation)
                (do (rollback! store memory-pool provider charge allocation nil nil nil)
                    {:error :injected-failure})
                (let [zero-result (phys/zero-pages! provider allocation)]
                  (if (:error zero-result)
                    (do (rollback! store memory-pool provider charge allocation nil nil nil)
                        {:error :provider-failure})
                    (let [object-result (oh/create-object! (:object-table store)
                                                          :memory-object
                                                          {:requested-size size
                                                           :backing-size backing-size
                                                           :page-count page-count})
                          object (:ok object-result)
                          object-id (:id object)]
                      (if (fail? store :after-object-construction)
                        (do (rollback! store memory-pool provider charge allocation nil object-id nil)
                            {:error :injected-failure})
                        (let [reservation-result (oh/reserve-slot! (:handle-table store))]
                          (if-let [reservation (:ok reservation-result)]
                            (if (fail? store :before-publish)
                              (do (rollback! store memory-pool provider charge allocation reservation object-id nil)
                                  {:error :injected-failure})
                              (let [publish-result (oh/publish! (:handle-table store)
                                                                (:object-table store)
                                                                reservation
                                                                object-id)]
                                (if-let [published (:ok publish-result)]
                                  (if (fail? store :after-publish)
                                    (do (rollback! store memory-pool provider charge allocation nil object-id (:handle published))
                                        {:error :injected-failure})
                                    (commit-record! store request size backing-size page-count
                                                    metadata-charge total-charge charge allocation
                                                    object-id published))
                                  (do (rollback! store memory-pool provider charge allocation reservation object-id nil)
                                      {:error :publish-failure}))))
                            (do (rollback! store memory-pool provider charge allocation nil object-id nil)
                                {:error :publish-failure}))))))))
              (do (pool/release! memory-pool charge)
                  {:error (:error allocation-result)}))))
        {:error (:error charge-result)}))))

(defn destroy! [store memory-pool provider object]
  (let [state (:state store)
        recorded (get-in @state [:objects (:id object)])]
    (if (not= recorded object)
      {:error :invalid-object}
      (let [unpublish-result (oh/unpublish! (:handle-table store) (:handle object))]
        (if (:error unpublish-result)
          {:error :publish-failure}
          (let [destroy-object-result (oh/destroy-object! (:object-table store) (:id object))]
            (if (:error destroy-object-result)
              {:error :invalid-object}
              (let [free-result (phys/free-pages! provider (:allocation object))]
                (if (:error free-result)
                  {:error :provider-failure}
                  (let [release-result (pool/release! memory-pool (:charge object))]
                    (if (:error release-result)
                      {:error :accounting-failure}
                      (do (swap! state update :objects dissoc (:id object))
                          {:ok nil}))))))))))))

(defn object-count [store] (count (:objects (store-state store))))
(defn live-handle-count [store] (oh/live-handle-count (:handle-table store)))
(defn reserved-handle-count [store] (oh/reserved-handle-count (:handle-table store)))
(defn kernel-object-count [store] (oh/object-count (:object-table store)))
(defn lookup-handle [store handle] (oh/lookup (:handle-table store) handle))