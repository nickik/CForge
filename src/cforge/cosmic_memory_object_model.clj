(ns cforge.cosmic-memory-object-model
  (:require [cforge.cosmic-memory-pool-model :as pool]
            [cforge.cosmic-physical-page-provider :as phys]))

;; Executable bootstrap oracle for the Forge MemoryObject creation transaction.
;; The production implementation belongs in Cosmic Forge code. This model exists
;; so failure atomicity can be tested before Forge can generate native kernel code.

(defn round-up [value align]
  (* (quot (+ value (dec align)) align) align))

(defn make-store
  ([] (make-store {}))
  ([{:keys [metadata-charge fail-stage]
     :or {metadata-charge 64 fail-stage nil}}]
   {:state (atom {:next-object-id 1
                  :objects {}
                  :metadata-charge metadata-charge
                  :fail-stage fail-stage})}))

(defn store-state [store] @(:state store))

(defn- fail? [store stage]
  (= stage (:fail-stage (store-state store))))

(defn- rollback! [memory-pool provider charge allocation]
  (when allocation
    (phys/free-pages! provider allocation))
  (when charge
    (pool/release! memory-pool charge))
  nil)

(defn create!
  [store memory-pool provider {:keys [size physically-contiguous?] :as request}]
  (let [{:keys [page-size]} (phys/geometry provider)
        metadata-charge (:metadata-charge (store-state store))]
    (cond
      (not (and (integer? size) (pos? size))) {:error :invalid-size}
      :else
      (let [backing-size (round-up size page-size)
            page-count (quot backing-size page-size)
            total-charge (+ backing-size metadata-charge)
            charge-result (pool/charge! memory-pool total-charge :memory-object)]
        (if-let [charge (:ok charge-result)]
          (if (fail? store :after-charge)
            (do (rollback! memory-pool provider charge nil)
                {:error :injected-failure})
            (let [allocation-result (phys/alloc-pages! provider page-count (boolean physically-contiguous?))]
              (if-let [allocation (:ok allocation-result)]
                (cond
                  (fail? store :after-physical-allocation)
                  (do (rollback! memory-pool provider charge allocation)
                      {:error :injected-failure})

                  :else
                  (let [zero-result (phys/zero-pages! provider allocation)]
                    (if (:error zero-result)
                      (do (rollback! memory-pool provider charge allocation)
                          {:error :provider-failure})
                      (if (fail? store :before-publish)
                        (do (rollback! memory-pool provider charge allocation)
                            {:error :injected-failure})
                        (let [state (:state store)
                              object-id (:next-object-id @state)
                              object {:id object-id
                                      :requested-size size
                                      :backing-size backing-size
                                      :page-count page-count
                                      :metadata-charge metadata-charge
                                      :total-charge total-charge
                                      :charge charge
                                      :allocation allocation
                                      :attributes (dissoc request :size)}]
                          (swap! state (fn [s]
                                         (-> s
                                             (update :next-object-id inc)
                                             (assoc-in [:objects object-id] object))))
                          {:ok object})))))
                (do
                  (pool/release! memory-pool charge)
                  {:error (:error allocation-result)}))))
          {:error (:error charge-result)})))))

(defn destroy! [store memory-pool provider object]
  (let [state (:state store)
        recorded (get-in @state [:objects (:id object)])]
    (if (not= recorded object)
      {:error :invalid-object}
      (let [free-result (phys/free-pages! provider (:allocation object))]
        (if (:error free-result)
          {:error :provider-failure}
          (let [release-result (pool/release! memory-pool (:charge object))]
            (if (:error release-result)
              {:error :accounting-failure}
              (do
                (swap! state update :objects dissoc (:id object))
                {:ok nil}))))))))

(defn object-count [store]
  (count (:objects (store-state store))))
