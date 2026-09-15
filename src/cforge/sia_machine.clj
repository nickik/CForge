(ns cforge.sia-machine)

;; Lowest-level hosted model of real SIA privileged effects. CForge's usize is
;; host-width, while native SIA32 usize is 32-bit, so every architectural value
;; is range-checked here. This is interpreter machinery, not Cosmic policy.

(def ^:private max-u32 4294967295N)
(def ^:private max-asid 4095N)

(def ^:dynamic *state*
  (atom {:status 0N
         :vmctx 0N
         :tlb-fence-all-count 0N
         :tlb-fence-va-count 0N
         :tlb-fence-asid-count 0N
         :last-tlb-fence-va 0N
         :last-tlb-fence-asid 0N}))

(defn- require-u32! [label value]
  (let [v (bigint value)]
    (when-not (<= 0N v max-u32)
      (throw (ex-info (str label " must fit SIA32") {:value value})))
    v))

(defn reset-machine! []
  (reset! *state* {:status 0N
                   :vmctx 0N
                   :tlb-fence-all-count 0N
                   :tlb-fence-va-count 0N
                   :tlb-fence-asid-count 0N
                   :last-tlb-fence-va 0N
                   :last-tlb-fence-asid 0N})
  nil)

(defn status-read [] (:status @*state*))
(defn status-write! [value]
  ;; SIA32-P defines STATUS bits 0..3; reserved bits read as zero.
  (let [v (require-u32! "STATUS" value)]
    (swap! *state* assoc :status (bit-and v 0x0fN)))
  nil)

(defn vmctx-read [] (:vmctx @*state*))
(defn vmctx-write! [value]
  (swap! *state* assoc :vmctx (require-u32! "VMCTX" value))
  nil)

(defn tlb-fence-all! []
  (swap! *state* update :tlb-fence-all-count inc)
  nil)

(defn tlb-fence-va! [va]
  (let [v (require-u32! "TLBFENCE.VA operand" va)]
    (swap! *state* (fn [s]
                     (-> s
                         (update :tlb-fence-va-count inc)
                         (assoc :last-tlb-fence-va v)))))
  nil)

(defn tlb-fence-asid! [asid]
  (let [v (bigint asid)]
    (when-not (<= 0N v max-asid)
      (throw (ex-info "TLBFENCE.ASID operand must fit 12 bits" {:value asid})))
    (swap! *state* (fn [s]
                     (-> s
                         (update :tlb-fence-asid-count inc)
                         (assoc :last-tlb-fence-asid v)))))
  nil)

(defn state [] @*state*)
