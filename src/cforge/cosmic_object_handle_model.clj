(ns cforge.cosmic-object-handle-model)

;; Bootstrap oracle for the minimal ObjectId + Handle publication layer.
;; This is deliberately smaller than the future full Cosmic Handle Space.

(defn make-object-table []
  {:state (atom {:next-object-id 1
                 :objects {}})})

(defn make-handle-table
  ([] (make-handle-table 64))
  ([capacity]
   {:state (atom {:capacity capacity
                  :slots (vec (repeat capacity {:generation 1
                                                :state :free
                                                :object-id nil}))})}))

(defn object-count [table]
  (count (:objects @(:state table))))

(defn live-handle-count [table]
  (count (filter #(= :published (:state %)) (:slots @(:state table)))))

(defn reserved-handle-count [table]
  (count (filter #(= :reserved (:state %)) (:slots @(:state table)))))

(defn create-object! [table object-type payload]
  (let [state (:state table)
        id (:next-object-id @state)
        object {:id id
                :object-type object-type
                :state :constructing
                :payload payload}]
    (swap! state (fn [s]
                   (-> s
                       (update :next-object-id inc)
                       (assoc-in [:objects id] object))))
    {:ok object}))

(defn object [table id]
  (get-in @(:state table) [:objects id]))

(defn mark-live! [table id]
  (let [state (:state table)
        current (get-in @state [:objects id])]
    (if (and current (= :constructing (:state current)))
      (do
        (swap! state assoc-in [:objects id :state] :live)
        {:ok (get-in @state [:objects id])})
      {:error :wrong-state})))

(defn destroy-object! [table id]
  (let [state (:state table)
        current (get-in @state [:objects id])]
    (if (and current (contains? #{:constructing :live} (:state current)))
      (do
        (swap! state update :objects dissoc id)
        {:ok nil})
      {:error :invalid-object})))

(defn reserve-slot! [table]
  (let [state (:state table)
        slots (:slots @state)
        index (first (keep-indexed (fn [i slot]
                                     (when (= :free (:state slot)) i))
                                   slots))]
    (if (nil? index)
      {:error :table-full}
      (let [generation (get-in @state [:slots index :generation])
            reservation {:slot index :generation generation}]
        (swap! state assoc-in [:slots index :state] :reserved)
        {:ok reservation}))))

(defn cancel-reservation! [table reservation]
  (let [state (:state table)
        {:keys [slot generation]} reservation
        current (get-in @state [:slots slot])]
    (if (and current
             (= generation (:generation current))
             (= :reserved (:state current)))
      (do
        (swap! state assoc-in [:slots slot]
               {:generation generation :state :free :object-id nil})
        {:ok nil})
      {:error :invalid-reservation})))

(defn publish! [table reservation object-id]
  (let [state (:state table)
        {:keys [slot generation]} reservation
        current (get-in @state [:slots slot])]
    (if (and current
             (= generation (:generation current))
             (= :reserved (:state current)))
      (let [handle {:slot slot :generation generation}]
        (swap! state assoc-in [:slots slot]
               {:generation generation :state :published :object-id object-id})
        {:ok {:handle handle :object-id object-id}})
      {:error :invalid-reservation})))

(defn lookup [table handle]
  (let [{:keys [slot generation]} handle
        current (get-in @(:state table) [:slots slot])]
    (if (and current
             (= generation (:generation current))
             (= :published (:state current)))
      {:ok (:object-id current)}
      {:error :invalid-handle})))

(defn unpublish! [table handle]
  (let [state (:state table)
        {:keys [slot generation]} handle
        current (get-in @state [:slots slot])]
    (if (and current
             (= generation (:generation current))
             (= :published (:state current)))
      (let [next-generation (inc generation)]
        (swap! state assoc-in [:slots slot]
               {:generation next-generation :state :free :object-id nil})
        {:ok nil})
      {:error :invalid-handle})))
