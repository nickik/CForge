(ns cforge.sia-machine
  (:require [clojure.string :as str]))

;; Lowest-level hosted model of real SIA privileged effects. CForge's usize is
;; host-width, while native SIA32 usize is 32-bit, so every architectural value
;; is range-checked here. This is interpreter machinery, not Cosmic policy.
;;
;; M6 also records a deterministic, versioned observation stream. The stream is
;; test-harness output only: it is deliberately NOT exposed as a Forge builtin
;; and therefore cannot become part of the native SIA ABI. LightingSimulation
;; can later emit the same format around a natively compiled Forge test.

(def ^:private max-u32 4294967295N)
(def ^:private max-asid 4095N)
(def observation-schema "sia32-machine-observation-v1")

(defn- fresh-state [status vmctx]
  {:status status
   :vmctx vmctx
   :tlb-fence-all-count 0N
   :tlb-fence-va-count 0N
   :tlb-fence-asid-count 0N
   :last-tlb-fence-va 0N
   :last-tlb-fence-asid 0N
   :next-event-seq 0N
   :events []})

(def ^:dynamic *state* (atom (fresh-state 0N 0N)))

(defn- require-u32! [label value]
  (let [v (bigint value)]
    (when-not (<= 0N v max-u32)
      (throw (ex-info (str label " must fit SIA32") {:value value})))
    v))

(defn- require-asid! [label value]
  (let [v (bigint value)]
    (when-not (<= 0N v max-asid)
      (throw (ex-info (str label " must fit 12 bits") {:value value})))
    v))

(defn- current-asid [state]
  (mod (:vmctx state) 4096N))

(defn- record-event [state event]
  (let [seq (:next-event-seq state)]
    (-> state
        (update :events conj (assoc event :seq seq))
        (update :next-event-seq inc))))

(defn reset-machine! []
  (reset! *state* (fresh-state 0N 0N))
  nil)

(defn initialize-machine!
  "Set deterministic initial architectural state without recording instructions.
   Intended for the external conformance harness before a Forge program runs."
  [status vmctx]
  (let [status-value (bit-and (require-u32! "STATUS" status) 0x0fN)
        vmctx-value (require-u32! "VMCTX" vmctx)]
    (reset! *state* (fresh-state status-value vmctx-value)))
  nil)

(defn status-read [] (:status @*state*))
(defn status-write! [value]
  ;; SIA32-P defines STATUS bits 0..3; reserved bits read as zero.
  (let [requested (require-u32! "STATUS" value)
        stored (bit-and requested 0x0fN)]
    (swap! *state*
           (fn [s]
             (-> s
                 (assoc :status stored)
                 (record-event {:op :status-write
                                :requested requested
                                :stored stored})))))
  nil)

(defn vmctx-read [] (:vmctx @*state*))
(defn vmctx-write! [value]
  (let [v (require-u32! "VMCTX" value)]
    (swap! *state*
           (fn [s]
             (-> s
                 (assoc :vmctx v)
                 (record-event {:op :vmctx-write :value v})))))
  nil)

(defn tlb-fence-all! []
  (swap! *state*
         (fn [s]
           (-> s
               (update :tlb-fence-all-count inc)
               (record-event {:op :tlb-fence-all}))))
  nil)

(defn tlb-fence-va! [va]
  (let [v (require-u32! "TLBFENCE.VA operand" va)]
    (swap! *state*
           (fn [s]
             (let [asid (current-asid s)]
               (-> s
                   (update :tlb-fence-va-count inc)
                   (assoc :last-tlb-fence-va v)
                   (record-event {:op :tlb-fence-va :va v :asid asid}))))))
  nil)

(defn tlb-fence-asid! [asid]
  (let [v (require-asid! "TLBFENCE.ASID operand" asid)]
    (swap! *state*
           (fn [s]
             (-> s
                 (update :tlb-fence-asid-count inc)
                 (assoc :last-tlb-fence-asid v)
                 (record-event {:op :tlb-fence-asid :asid v})))))
  nil)

(defn state [] @*state*)

(defn observation []
  (let [s @*state*]
    {:schema observation-schema
     :status (:status s)
     :vmctx (:vmctx s)
     :events (:events s)}))

(defn- event-line [{:keys [seq op requested stored value va asid]}]
  (case op
    :status-write (str "event\t" seq "\tstatus_write\t" requested "\t" stored)
    :vmctx-write (str "event\t" seq "\tvmctx_write\t" value)
    :tlb-fence-all (str "event\t" seq "\ttlb_fence_all")
    :tlb-fence-va (str "event\t" seq "\ttlb_fence_va\t" va "\t" asid)
    :tlb-fence-asid (str "event\t" seq "\ttlb_fence_asid\t" asid)
    (throw (ex-info "unknown SIA observation event" {:event op}))))

(defn observation-text []
  (let [{:keys [schema status vmctx events]} (observation)]
    (str/join "\n"
              (concat [schema
                       (str "status\t" status)
                       (str "vmctx\t" vmctx)]
                      (map event-line events)
                      [""]))))
