(ns cforge.sia-machine)

;; Lowest-level hosted model of the SIA privileged effects that are real
;; architectural operations. This is interpreter machinery, not Cosmic policy.
;; Page-table construction/walking and address-space semantics remain Forge.

(def ^:dynamic *state*
  (atom {:status 0N
         :vmctx 0N
         :tlb-fence-all-count 0N
         :tlb-fence-va-count 0N
         :tlb-fence-asid-count 0N
         :last-tlb-fence-va 0N
         :last-tlb-fence-asid 0N}))

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
  ;; SIA32-P currently defines STATUS bits 0..3; reserved bits read as zero.
  (swap! *state* assoc :status (bit-and (bigint value) 0x0fN))
  nil)

(defn vmctx-read [] (:vmctx @*state*))
(defn vmctx-write! [value]
  ;; VMCTX is a full 32-bit architectural register. CForge integer checking
  ;; already ensures the Forge u32 caller cannot supply a wider value.
  (swap! *state* assoc :vmctx (bigint value))
  nil)

(defn tlb-fence-all! []
  (swap! *state* update :tlb-fence-all-count inc)
  nil)

(defn tlb-fence-va! [va]
  (swap! *state* (fn [s]
                   (-> s
                       (update :tlb-fence-va-count inc)
                       (assoc :last-tlb-fence-va (bigint va)))))
  nil)

(defn tlb-fence-asid! [asid]
  (swap! *state* (fn [s]
                   (-> s
                       (update :tlb-fence-asid-count inc)
                       (assoc :last-tlb-fence-asid (bigint asid)))))
  nil)

(defn state [] @*state*)
