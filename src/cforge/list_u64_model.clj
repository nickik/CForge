(ns cforge.list-u64-model)

(defn make-allocator
  "Deterministic allocator domain used by the ListU64 semantic reference.
   max-bytes bounds live storage. Blocks carry allocator provenance; the list
   itself never retains an allocator capability or allocator identity."
  [id max-bytes]
  {:id id
   :max-bytes max-bytes
   :state (atom {:next-block 1
                 :live-bytes 0
                 :blocks {}})})

(defn allocator-state [allocator]
  @(:state allocator))

(defn- alloc-block [allocator bytes]
  (let [{:keys [live-bytes next-block]} @(:state allocator)]
    (if (> (+ live-bytes bytes) (:max-bytes allocator))
      {:error :out-of-memory}
      (let [block {:id next-block
                   :owner (:id allocator)
                   :bytes bytes
                   :values (vec (repeat (quot bytes 8) 0N))}]
        (swap! (:state allocator)
               (fn [s]
                 (-> s
                     (update :next-block inc)
                     (update :live-bytes + bytes)
                     (assoc-in [:blocks next-block] block))))
        {:ok block}))))

(defn- resize-block [allocator block new-bytes]
  (cond
    (not= (:owner block) (:id allocator))
    {:error :foreign-allocation}

    :else
    (let [old-bytes (:bytes block)
          delta (- new-bytes old-bytes)
          live (:live-bytes @(:state allocator))]
      (if (> (+ live delta) (:max-bytes allocator))
        {:error :out-of-memory}
        (let [slots (quot new-bytes 8)
              old-values (:values block)
              values (vec (take slots (concat old-values (repeat 0N))))
              replacement (assoc block :bytes new-bytes :values values)]
          (swap! (:state allocator)
                 (fn [s]
                   (-> s
                       (update :live-bytes + delta)
                       (assoc-in [:blocks (:id block)] replacement))))
          {:ok replacement})))))

(defn- free-block [allocator block]
  (if (not= (:owner block) (:id allocator))
    {:error :foreign-allocation}
    (do
      (swap! (:state allocator)
             (fn [s]
               (-> s
                   (update :live-bytes - (:bytes block))
                   (update :blocks dissoc (:id block)))))
      {:ok nil})))

(defn create []
  {:block nil :len 0 :capacity 0})

(defn with-capacity [allocator capacity]
  (if (zero? capacity)
    {:ok (create)}
    (let [result (alloc-block allocator (* capacity 8))]
      (if-let [block (:ok result)]
        {:ok {:block block :len 0 :capacity capacity}}
        result))))

(defn len [list] (:len list))
(defn capacity [list] (:capacity list))
(defn empty? [list] (zero? (:len list)))

(defn next-capacity [current requested]
  (loop [next (if (zero? current) 4 current)]
    (if (>= next requested) next (recur (* next 2)))))

(defn reserve [list allocator requested]
  (if (<= requested (:capacity list))
    {:ok list}
    (let [new-capacity (next-capacity (:capacity list) requested)
          new-bytes (* new-capacity 8)
          result (if (zero? (:capacity list))
                   (alloc-block allocator new-bytes)
                   (resize-block allocator (:block list) new-bytes))]
      (if-let [block (:ok result)]
        {:ok (assoc list :block block :capacity new-capacity)}
        ;; Transactional failure: return the original value untouched.
        (assoc result :list list)))))

(defn- store [list index value]
  (assoc-in list [:block :values index] (bigint value)))

(defn get-at [list index]
  (when-not (< index (:len list))
    (throw (ex-info "ListU64 index out of bounds" {:index index :len (:len list)})))
  (get-in list [:block :values index]))

(defn set-at [list index value]
  (when-not (< index (:len list))
    (throw (ex-info "ListU64 index out of bounds" {:index index :len (:len list)})))
  (store list index value))

(defn push [list allocator value]
  (let [grown (if (= (:len list) (:capacity list))
                (reserve list allocator (inc (:len list)))
                {:ok list})]
    (if-let [ready (:ok grown)]
      {:ok (-> ready
               (store (:len ready) value)
               (update :len inc))}
      grown)))

(defn insert [list allocator index value]
  (when-not (<= index (:len list))
    (throw (ex-info "ListU64 insert index out of bounds" {:index index :len (:len list)})))
  (let [grown (if (= (:len list) (:capacity list))
                (reserve list allocator (inc (:len list)))
                {:ok list})]
    (if-let [ready (:ok grown)]
      (let [old-values (get-in ready [:block :values])
            n (:len ready)
            shifted (reduce (fn [values i]
                              (assoc values i (nth values (dec i))))
                            old-values
                            (range n index -1))]
        {:ok (-> ready
                 (assoc-in [:block :values] (assoc shifted index (bigint value)))
                 (update :len inc))})
      grown)))

(defn pop [list]
  (if (zero? (:len list))
    {:value nil :list list}
    (let [index (dec (:len list))]
      {:value (get-in list [:block :values index])
       :list (assoc list :len index)})))

(defn remove-at [list index]
  (when-not (< index (:len list))
    (throw (ex-info "ListU64 index out of bounds" {:index index :len (:len list)})))
  (let [removed (get-at list index)
        n (:len list)
        values (reduce (fn [values i]
                         (assoc values i (nth values (inc i))))
                       (get-in list [:block :values])
                       (range index (dec n)))]
    {:value removed
     :list (-> list
               (assoc-in [:block :values] values)
               (update :len dec))}))

(defn swap-remove [list index]
  (when-not (< index (:len list))
    (throw (ex-info "ListU64 index out of bounds" {:index index :len (:len list)})))
  (let [removed (get-at list index)
        last-index (dec (:len list))
        last-value (get-at list last-index)
        list' (if (= index last-index) list (store list index last-value))]
    {:value removed :list (assoc list' :len last-index)}))

(defn clear [list]
  (assoc list :len 0))

(defn truncate [list new-len]
  (if (< new-len (:len list)) (assoc list :len new-len) list))

(defn destroy [list allocator]
  (if (zero? (:capacity list))
    {:ok (create)}
    (let [result (free-block allocator (:block list))]
      (if (:error result)
        (assoc result :list list)
        {:ok (create)}))))
