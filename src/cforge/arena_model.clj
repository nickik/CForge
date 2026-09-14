(ns cforge.arena-model
  (:require [clojure.set :as set]))

(defn power-of-two? [n]
  (and (integer? n) (pos? n) (zero? (bit-and n (dec n)))))

(defn align-up [value align]
  (bit-and (+ value (dec align)) (bit-not (dec align))))

(defn valid-request? [{:keys [size align]}]
  (and (integer? size) (pos? size) (power-of-two? align)))

(defn- linear-alloc [state request]
  (if-not (valid-request? request)
    {:error (if (or (nil? (:size request)) (not (pos? (:size request 0))))
              :invalid-size
              :invalid-alignment)}
    (let [{:keys [next capacity base]} @state
          start (align-up next (:align request))
          end (+ start (:size request))]
      (swap! state update :requests conj request)
      (if (> end capacity)
        {:error :out-of-memory}
        (let [block {:address (+ base start)
                     :size (:size request)
                     :align (:align request)}]
          (swap! state (fn [s]
                         (-> s
                             (assoc :next end)
                             (assoc-in [:allocations (:address block)] block))))
          {:ok block})))))

(defn- linear-free [state block]
  (swap! state (fn [s]
                 (-> s
                     (update :allocations dissoc (:address block))
                     (update :freed-bytes + (:size block)))))
  nil)

(defn- linear-reclaim [state target-bytes]
  ;; The deterministic provider has no hidden cache of its own. Freeing an
  ;; extent already makes it provider-owned again, so provider-level reclaim is
  ;; observability only. ObjectCache reclaim is tested separately.
  (swap! state update :reclaim-requests conj target-bytes)
  0)

(defn make-linear-arena
  "Create a deterministic provider-backed Arena model. `kind` is deliberately
   metadata only: core algorithms must behave identically for :kernel and
   :hosted providers. Addresses are integer stand-ins for raw pointers."
  ([kind capacity] (make-linear-arena kind capacity 0x10000000))
  ([kind capacity base]
   (let [state (atom {:kind kind
                      :capacity capacity
                      :base base
                      :next 0
                      :allocations {}
                      :freed-bytes 0
                      :requests []
                      :reclaim-requests []})]
     {:kind kind
      :context state
      :ops {:alloc linear-alloc
            :free linear-free
            :reclaim linear-reclaim}})))

(defn arena-state [arena]
  @(:context arena))

(defn arena-alloc [arena request]
  ((get-in arena [:ops :alloc]) (:context arena) request))

(defn arena-free [arena block]
  ((get-in arena [:ops :free]) (:context arena) block))

(defn arena-reclaim [arena target-bytes]
  ((get-in arena [:ops :reclaim]) (:context arena) target-bytes))

(defn make-arena-allocator [arena]
  {:context arena
   :ops {:alloc (fn [a request] (arena-alloc a request))
         :free (fn [a block] (arena-free a block))
         :resize (fn [a block new-size]
                   (if-not (pos? new-size)
                     {:error :invalid-size}
                     (let [result (arena-alloc a {:size new-size
                                                  :align (:align block)
                                                  :wait :may-wait})]
                       (when (:ok result)
                         (arena-free a block))
                       result)))}})

(defn allocator-alloc [allocator request]
  ((get-in allocator [:ops :alloc]) (:context allocator) request))

(defn allocator-free [allocator block]
  ((get-in allocator [:ops :free]) (:context allocator) block))

(defn allocator-resize [allocator block new-size]
  ((get-in allocator [:ops :resize]) (:context allocator) block new-size))

(defn valid-object-cache-spec? [{:keys [object-size object-align slab-size]}]
  (and (integer? object-size)
       (pos? object-size)
       (power-of-two? object-align)
       (power-of-two? slab-size)
       (>= slab-size object-size)
       (<= (align-up object-size object-align) slab-size)))

(defn object-stride [{:keys [object-size object-align]}]
  (align-up object-size object-align))

(defn objects-per-slab [spec]
  (quot (:slab-size spec) (object-stride spec)))

(defn make-object-cache [arena spec]
  (if-not (valid-object-cache-spec? spec)
    {:error :invalid-spec}
    {:ok {:arena arena
          :spec spec
          :state (atom {:slabs []
                        :free []
                        :in-use #{}
                        :allocations 0
                        :frees 0
                        :allocation-failures 0
                        :reclaims 0})}}))

(defn- add-slab! [cache wait]
  (let [spec (:spec cache)
        result (arena-alloc (:arena cache)
                            {:size (:slab-size spec)
                             :align (:slab-size spec)
                             :wait wait})]
    (if-let [block (:ok result)]
      (let [stride (object-stride spec)
            count (objects-per-slab spec)
            objects (mapv #(+ (:address block) (* % stride)) (range count))
            slab {:block block :objects (set objects)}]
        (swap! (:state cache)
               (fn [s]
                 (-> s
                     (update :slabs conj slab)
                     (update :free into objects))))
        {:ok slab})
      (do
        (swap! (:state cache) update :allocation-failures inc)
        result))))

(defn object-cache-alloc [cache wait]
  (when (empty? (:free @(:state cache)))
    (add-slab! cache wait))
  (let [state (:state cache)
        address (peek (:free @state))]
    (if (nil? address)
      {:error :out-of-memory}
      (do
        (swap! state (fn [s]
                       (-> s
                           (update :free pop)
                           (update :in-use conj address)
                           (update :allocations inc))))
        {:ok address}))))

(defn object-cache-free [cache address]
  (let [state (:state cache)]
    (if-not (contains? (:in-use @state) address)
      {:error :invalid-free}
      (do
        (swap! state (fn [s]
                       (-> s
                           (update :in-use disj address)
                           (update :free conj address)
                           (update :frees inc))))
        {:ok nil}))))

(defn object-cache-reclaim [cache]
  (let [state (:state cache)
        before @state
        reclaimable (vec (filter (fn [slab]
                                   (not-any? (:in-use before) (:objects slab)))
                                 (:slabs before)))
        reclaimed-set (set reclaimable)
        reclaimed-objects (reduce set/union #{} (map :objects reclaimable))
        reclaimed-bytes (reduce + 0 (map #(get-in % [:block :size]) reclaimable))]
    (doseq [slab reclaimable]
      (arena-free (:arena cache) (:block slab)))
    (swap! state (fn [s]
                   (-> s
                       (assoc :slabs (vec (remove reclaimed-set (:slabs s))))
                       (assoc :free (vec (remove reclaimed-objects (:free s))))
                       (update :reclaims inc))))
    reclaimed-bytes))

(defn object-cache-stats [cache]
  (let [s @(:state cache)]
    {:allocations (:allocations s)
     :frees (:frees s)
     :allocation-failures (:allocation-failures s)
     :objects-in-use (count (:in-use s))
     :objects-free (count (:free s))
     :slabs-total (count (:slabs s))
     :bytes-backing (reduce + 0 (map #(get-in % [:block :size]) (:slabs s)))
     :reclaims (:reclaims s)}))
