(ns cforge.cosmic-physical-page-provider)

;; Bootstrap implementation of Cosmic's PhysicalMemoryProvider contract.
;; This is intentionally the replaceable machine-facing layer. Cosmic policy
;; must not depend on this Clojure representation.

(defn- valid-positive-int? [n]
  (and (integer? n) (pos? n)))

(defn make-provider
  ([page-size total-pages]
   (make-provider page-size total-pages {}))
  ([page-size total-pages {:keys [base fail-after-allocations]
                           :or {base 0 fail-after-allocations nil}}]
   (when-not (and (valid-positive-int? page-size)
                  (zero? (bit-and page-size (dec page-size))))
     (throw (ex-info "page-size must be a positive power of two" {:page-size page-size})))
   (when-not (valid-positive-int? total-pages)
     (throw (ex-info "total-pages must be positive" {:total-pages total-pages})))
   {:state (atom {:page-size page-size
                  :total-pages total-pages
                  :base base
                  :reserved #{}
                  :allocated {}
                  :next-allocation-id 1
                  :allocation-attempts 0
                  :fail-after-allocations fail-after-allocations
                  :zeroed-pages #{}})}))

(defn provider-state [provider] @(:state provider))

(defn geometry [provider]
  (let [{:keys [page-size total-pages]} (provider-state provider)]
    {:page-size page-size :total-pages total-pages}))

(defn- free-page? [s page]
  (and (<= 0 page)
       (< page (:total-pages s))
       (not (contains? (:reserved s) page))
       (not-any? #(contains? (:pages %) page) (vals (:allocated s)))))

(defn reserve-pages! [provider first-page count]
  (let [state (:state provider)
        s @state]
    (cond
      (not (and (integer? first-page) (<= 0 first-page) (valid-positive-int? count)))
      {:error :invalid-size}

      (> (+ first-page count) (:total-pages s))
      {:error :invalid-size}

      (not-every? #(free-page? s %) (range first-page (+ first-page count)))
      {:error :already-reserved}

      :else
      (do
        (swap! state update :reserved into (range first-page (+ first-page count)))
        {:ok nil}))))

(defn reserve-range! [provider base size]
  (let [{:keys [page-size base provider-base] :as _g}
        (assoc (geometry provider) :provider-base (:base (provider-state provider)))
        provider-base (:base (provider-state provider))]
    (cond
      (not (valid-positive-int? size)) {:error :invalid-size}
      (or (not (zero? (mod (- base provider-base) page-size)))
          (not (zero? (mod size page-size)))) {:error :invalid-alignment}
      :else (reserve-pages! provider
                            (quot (- base provider-base) page-size)
                            (quot size page-size)))))

(defn- contiguous-run [s count]
  (first
   (for [start (range 0 (inc (- (:total-pages s) count)))
         :let [pages (vec (range start (+ start count)))]
         :when (every? #(free-page? s %) pages)]
     pages)))

(defn alloc-pages! [provider count contiguous?]
  (let [state (:state provider)
        s @state
        attempts (:allocation-attempts s)
        fail-after (:fail-after-allocations s)]
    (swap! state update :allocation-attempts inc)
    (cond
      (not (valid-positive-int? count)) {:error :invalid-size}
      (and fail-after (>= attempts fail-after)) {:error :out-of-memory}
      :else
      (let [pages (if contiguous?
                    (contiguous-run s count)
                    (vec (take count (filter #(free-page? s %) (range (:total-pages s))))))]
        (if (not= count (count pages))
          {:error (if contiguous? :unsuitable-memory :out-of-memory)}
          (let [id (:next-allocation-id s)
                allocation {:id id
                            :pages (set pages)
                            :ordered-pages pages
                            :count count
                            :contiguous contiguous?}]
            (swap! state (fn [v]
                           (-> v
                               (update :next-allocation-id inc)
                               (assoc-in [:allocated id] allocation))))
            {:ok allocation}))))))

(defn free-pages! [provider allocation]
  (let [state (:state provider)
        recorded (get-in @state [:allocated (:id allocation)])]
    (if (= recorded allocation)
      (do
        (swap! state (fn [s]
                       (-> s
                           (update :allocated dissoc (:id allocation))
                           (update :zeroed-pages #(apply disj % (:pages allocation))))))
        {:ok nil})
      {:error :not-allocated})))

(defn zero-pages! [provider allocation]
  (let [state (:state provider)
        recorded (get-in @state [:allocated (:id allocation)])]
    (if (= recorded allocation)
      (do
        (swap! state update :zeroed-pages into (:pages allocation))
        {:ok nil})
      {:error :not-allocated})))

(defn allocation-zeroed? [provider allocation]
  (let [zeroed (:zeroed-pages (provider-state provider))]
    (every? zeroed (:pages allocation))))

(defn stats [provider]
  (let [s (provider-state provider)
        allocated-pages (reduce + 0 (map :count (vals (:allocated s))))
        reserved-pages (count (:reserved s))]
    {:total-pages (:total-pages s)
     :reserved-pages reserved-pages
     :allocated-pages allocated-pages
     :free-pages (- (:total-pages s) reserved-pages allocated-pages)}))

(defn valid-invariants? [provider]
  (let [s (provider-state provider)
        allocations (vals (:allocated s))
        allocated-pages (mapcat :pages allocations)
        allocated-set (set allocated-pages)]
    (and (= (count allocated-pages) (count allocated-set))
         (empty? (clojure.set/intersection (:reserved s) allocated-set))
         (= (:total-pages s)
            (+ (count (:reserved s))
               (count allocated-set)
               (:free-pages (stats provider)))))))
