(ns cforge.cosmic-memory-pool-model)

;; Host-executable reference model for Cosmic's v0.1 MemoryPool semantics.
;;
;; This is deliberately NOT a physical-memory allocator. A MemoryPool is
;; allocation authority plus accounting. Physical placement belongs to a
;; separate kernel allocator below this layer.
;;
;; v0.1 uses reserved delegation: deriving a child reserves its full limit from
;; the parent. This makes delegation deterministic and prevents a hierarchy from
;; manufacturing more guaranteed allocation authority than its root owns.

(defn valid-size? [n]
  (and (integer? n) (pos? n)))

(defn make-root-pool
  "Create a root MemoryPool with a finite byte limit. Addresses and physical
   memory do not appear in this model."
  [limit]
  (when-not (valid-size? limit)
    (throw (ex-info "MemoryPool limit must be positive" {:limit limit})))
  {:state (atom {:limit limit
                 :charged 0
                 :reserved 0
                 :next-child-id 1
                 :next-charge-id 1
                 :children {}
                 :charges {}
                 :parent nil
                 :reservation 0
                 :revoked? false})})

(defn pool-state [pool]
  @(:state pool))

(defn available [pool]
  (let [{:keys [limit charged reserved]} (pool-state pool)]
    (- limit charged reserved)))

(defn usage [pool]
  (let [{:keys [limit charged reserved revoked?]} (pool-state pool)]
    {:limit limit
     :charged charged
     :reserved reserved
     :available (- limit charged reserved)
     :revoked? revoked?}))

(defn- live-pool? [pool]
  (not (:revoked? (pool-state pool))))

(defn derive!
  "Reserve `limit` bytes of the parent and create a child pool. The reservation
   is capacity authority, not an allocation and not physical memory."
  [parent limit]
  (cond
    (not (valid-size? limit)) {:error :invalid-size}
    (not (live-pool? parent)) {:error :revoked}
    (> limit (available parent)) {:error :limit-exceeded}
    :else
    (let [parent-state (:state parent)
          child-id (:next-child-id @parent-state)
          child {:state (atom {:limit limit
                               :charged 0
                               :reserved 0
                               :next-child-id 1
                               :next-charge-id 1
                               :children {}
                               :charges {}
                               :parent parent
                               :parent-child-id child-id
                               :reservation limit
                               :revoked? false})}]
      (swap! parent-state
             (fn [s]
               (-> s
                   (update :reserved + limit)
                   (update :next-child-id inc)
                   (assoc-in [:children child-id] child))))
      {:ok child})))

(defn charge!
  "Charge bytes to a pool and return a charge token. The token is later used to
   release exactly this charge; it is not a pointer or object handle."
  ([pool bytes] (charge! pool bytes :unspecified))
  ([pool bytes kind]
   (cond
     (not (valid-size? bytes)) {:error :invalid-size}
     (not (live-pool? pool)) {:error :revoked}
     (> bytes (available pool)) {:error :limit-exceeded}
     :else
     (let [state (:state pool)
           charge-id (:next-charge-id @state)
           charge {:id charge-id :bytes bytes :kind kind}]
       (swap! state
              (fn [s]
                (-> s
                    (update :charged + bytes)
                    (update :next-charge-id inc)
                    (assoc-in [:charges charge-id] charge))))
       {:ok charge}))))

(defn release!
  "Release one prior charge. Double release and foreign/unknown tokens fail."
  [pool charge]
  (let [state (:state pool)
        charge-id (:id charge)
        recorded (get-in @state [:charges charge-id])]
    (cond
      (nil? recorded) {:error :invalid-charge}
      (not= recorded charge) {:error :invalid-charge}
      :else
      (do
        (swap! state
               (fn [s]
                 (-> s
                     (update :charged - (:bytes recorded))
                     (update :charges dissoc charge-id))))
        {:ok nil}))))

(defn destroy-child!
  "Destroy an empty child pool and return its reservation to its parent.
   v0.1 deliberately refuses implicit destruction of live charges or children."
  [child]
  (let [{:keys [parent parent-child-id reservation charged reserved children revoked?]}
        (pool-state child)]
    (cond
      (nil? parent) {:error :root-pool}
      revoked? {:error :revoked}
      (pos? charged) {:error :pool-in-use}
      (pos? reserved) {:error :pool-in-use}
      (seq children) {:error :pool-in-use}
      :else
      (do
        (swap! (:state child) assoc :revoked? true)
        (swap! (:state parent)
               (fn [s]
                 (-> s
                     (update :reserved - reservation)
                     (update :children dissoc parent-child-id))))
        {:ok nil}))))

(defn valid-invariants?
  "Local invariant checker used by tests and later differential models."
  [pool]
  (let [{:keys [limit charged reserved charges children]} (pool-state pool)
        charge-sum (reduce + 0 (map :bytes (vals charges)))
        child-reservation-sum
        (reduce + 0 (map #(-> % pool-state :reservation) (vals children)))]
    (and (>= limit 0)
         (>= charged 0)
         (>= reserved 0)
         (= charged charge-sum)
         (= reserved child-reservation-sum)
         (<= (+ charged reserved) limit))))
