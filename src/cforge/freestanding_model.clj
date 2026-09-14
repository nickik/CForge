(ns cforge.freestanding-model
  (:require [clojure.string :as str]))

;; Executable semantic model for Forge's freestanding OS-foundation libraries.
;; It deliberately uses plain Clojure data instead of JVM interfaces so the
;; contracts remain close to Forge's explicit context + ops style.

;; ---- core.mem -------------------------------------------------------------

(defn ranges-overlap? [a-start a-len b-start b-len]
  (and (pos? a-len)
       (pos? b-len)
       (< a-start (+ b-start b-len))
       (< b-start (+ a-start a-len))))

(defn mem-set [memory start value len]
  (reduce (fn [m i] (assoc m i (bit-and value 0xff)))
          (vec memory)
          (range start (+ start len))))

(defn mem-move [memory dst src len]
  (let [m (vec memory)
        chunk (subvec m src (+ src len))]
    (reduce-kv (fn [out i value] (assoc out (+ dst i) value))
               m chunk)))

(defn mem-copy-nonoverlapping [memory dst src len]
  (if (ranges-overlap? dst len src len)
    {:error :overlap}
    {:ok (mem-move memory dst src len)}))

(defn mem-compare [a b]
  (loop [xs (seq a) ys (seq b)]
    (cond
      (and (nil? xs) (nil? ys)) 0
      (nil? xs) -1
      (nil? ys) 1
      (< (first xs) (first ys)) -1
      (> (first xs) (first ys)) 1
      :else (recur (next xs) (next ys)))))

;; ---- core.bits ------------------------------------------------------------

(defn u32 [value] (bit-and value 0xffffffff))

(defn bswap-u16 [value]
  (bit-and 0xffff
           (bit-or (bit-shift-left (bit-and value 0xff) 8)
                   (unsigned-bit-shift-right (bit-and value 0xff00) 8))))

(defn bswap-u32 [value]
  (u32 (bit-or (bit-shift-left (bit-and value 0x000000ff) 24)
               (bit-shift-left (bit-and value 0x0000ff00) 8)
               (unsigned-bit-shift-right (bit-and value 0x00ff0000) 8)
               (unsigned-bit-shift-right (bit-and value 0xff000000) 24))))

(defn low-mask-u32 [width]
  (cond
    (zero? width) 0
    (>= width 32) 0xffffffff
    :else (dec (bit-shift-left 1 width))))

(defn extract-u32 [value lsb width]
  (bit-and (unsigned-bit-shift-right value lsb) (low-mask-u32 width)))

(defn insert-u32 [base field lsb width]
  (let [mask (u32 (bit-shift-left (low-mask-u32 width) lsb))]
    (u32 (bit-or (bit-and base (bit-not mask))
                 (bit-and (bit-shift-left field lsb) mask)))))

(defn encode-u32-le [value]
  (mapv #(bit-and 0xff (unsigned-bit-shift-right value %)) [0 8 16 24]))

(defn decode-u32-le [bytes]
  (u32 (reduce bit-or 0
               (map-indexed #(bit-shift-left %2 (* 8 %1)) (take 4 bytes)))))

(defn encode-u32-be [value] (vec (reverse (encode-u32-le value))))
(defn decode-u32-be [bytes] (decode-u32-le (reverse bytes)))

;; ---- core.mmio ------------------------------------------------------------

(defn make-mmio [initial]
  {:registers (atom (into {} initial))
   :log (atom [])})

(defn mmio-read [bus address]
  (swap! (:log bus) conj [:read address])
  (get @(:registers bus) address 0))

(defn mmio-write! [bus address value]
  (swap! (:log bus) conj [:write address value])
  (swap! (:registers bus) assoc address value)
  nil)

;; ---- core.atomic ----------------------------------------------------------

(def memory-orders #{:relaxed :acquire :release :acq-rel :seq-cst})

(defn valid-memory-order? [order] (contains? memory-orders order))

(defn make-atomic [initial]
  {:value (atom initial)
   :log (atom [])})

(defn atomic-load [a order]
  (assert (valid-memory-order? order))
  (swap! (:log a) conj [:load order])
  @(:value a))

(defn atomic-store! [a value order]
  (assert (valid-memory-order? order))
  (swap! (:log a) conj [:store order value])
  (reset! (:value a) value)
  nil)

(defn atomic-fetch-add! [a delta order]
  (assert (valid-memory-order? order))
  (let [old @(:value a)]
    (swap! (:log a) conj [:fetch-add order delta])
    (swap! (:value a) + delta)
    old))

(defn atomic-compare-exchange! [a expected desired success-order failure-order]
  (assert (valid-memory-order? success-order))
  (assert (valid-memory-order? failure-order))
  (let [old @(:value a)
        exchanged? (= old expected)]
    (swap! (:log a) conj [:compare-exchange success-order failure-order expected desired])
    (when exchanged? (reset! (:value a) desired))
    {:old old :exchanged? exchanged?}))

;; ---- core.sync ------------------------------------------------------------

(defn make-spin-lock [] {:state (make-atomic 0)})

(defn spin-try-lock! [lock]
  (:exchanged? (atomic-compare-exchange! (:state lock) 0 1 :acquire :relaxed)))

(defn spin-unlock! [lock]
  (atomic-store! (:state lock) 0 :release))

(defn make-critical-section [enter-fn exit-fn context]
  {:context context :ops {:enter enter-fn :exit exit-fn}})

(defn critical-enter [section]
  ((get-in section [:ops :enter]) (:context section)))

(defn critical-exit [section token]
  ((get-in section [:ops :exit]) (:context section) token))

;; ---- core.fixed -----------------------------------------------------------

(defn make-fixed-vec [capacity]
  {:capacity capacity :data (atom [])})

(defn fixed-vec-push! [v value]
  (if (= (:capacity v) (count @(:data v)))
    {:error :full}
    (do (swap! (:data v) conj value) {:ok nil})))

(defn fixed-vec-pop! [v]
  (if (empty? @(:data v))
    {:error :empty}
    (let [value (peek @(:data v))]
      (swap! (:data v) pop)
      {:ok value})))

(defn fixed-vec-values [v] @(:data v))

(defn make-ring [capacity]
  {:capacity capacity
   :state (atom {:slots (vec (repeat capacity nil)) :head 0 :len 0})})

(defn ring-len [ring] (:len @(:state ring)))

(defn ring-push! [ring value]
  (let [{:keys [head len]} @(:state ring)
        capacity (:capacity ring)]
    (if (= len capacity)
      {:error :full}
      (let [index (mod (+ head len) capacity)]
        (swap! (:state ring) (fn [s]
                               (-> s
                                   (assoc-in [:slots index] value)
                                   (update :len inc))))
        {:ok nil}))))

(defn ring-pop! [ring]
  (let [{:keys [slots head len]} @(:state ring)
        capacity (:capacity ring)]
    (if (zero? len)
      {:error :empty}
      (let [value (nth slots head)]
        (swap! (:state ring) (fn [s]
                               (-> s
                                   (assoc-in [:slots head] nil)
                                   (assoc :head (mod (inc head) capacity))
                                   (update :len dec))))
        {:ok value}))))

;; ---- core.intrusive -------------------------------------------------------

(defn make-intrusive-queue []
  {:state (atom {:head nil :tail nil :len 0 :links {}})})

(defn intrusive-len [queue] (:len @(:state queue)))

(defn intrusive-enqueue! [queue id]
  (let [state (:state queue)
        {:keys [tail links]} @state]
    (if (contains? links id)
      {:error :already-linked}
      (do
        (swap! state
               (fn [s]
                 (let [s1 (assoc-in s [:links id] {:prev tail :next nil})
                       s2 (if tail (assoc-in s1 [:links tail :next] id) s1)]
                   (-> s2
                       (assoc :tail id)
                       (update :head #(or % id))
                       (update :len inc)))))
        {:ok nil}))))

(defn intrusive-dequeue! [queue]
  (let [state (:state queue)
        {:keys [head links]} @state]
    (if (nil? head)
      {:error :empty}
      (let [next-id (get-in links [head :next])]
        (swap! state
               (fn [s]
                 (let [s1 (update s :links dissoc head)
                       s2 (if next-id (assoc-in s1 [:links next-id :prev] nil) s1)]
                   (-> s2
                       (assoc :head next-id)
                       (cond-> (nil? next-id) (assoc :tail nil))
                       (update :len dec)))))
        {:ok head}))))

;; ---- core.layout ----------------------------------------------------------

(defn align-up [value align]
  (bit-and (+ value (dec align)) (bit-not (dec align))))

(defn layout-struct [fields]
  (loop [remaining fields offset 0 max-align 1 offsets []]
    (if-let [{:keys [size align]} (first remaining)]
      (let [field-offset (align-up offset align)]
        (recur (next remaining)
               (+ field-offset size)
               (max max-align align)
               (conj offsets field-offset)))
      {:offsets offsets
       :align max-align
       :size (align-up offset max-align)})))

;; ---- core.io --------------------------------------------------------------

(defn make-fixed-writer [capacity]
  {:capacity capacity :bytes (atom [])})

(defn writer-write-bytes! [writer bytes]
  (let [bytes (vec bytes)]
    (if (> (+ (count @(:bytes writer)) (count bytes)) (:capacity writer))
      {:error :full}
      (do (swap! (:bytes writer) into bytes) {:ok (count bytes)}))))

(defn utf8-bytes [text]
  (mapv #(bit-and (int %) 0xff) (.getBytes ^String text "UTF-8")))

(defn writer-write-str! [writer text]
  (writer-write-bytes! writer (utf8-bytes text)))

(defn writer-write-u64-dec! [writer value]
  (writer-write-str! writer (str value)))

(defn writer-write-u64-hex! [writer value]
  (writer-write-str! writer (str "0x" (Long/toHexString (long value)))))

(defn writer-string [writer]
  (String. (byte-array (map unchecked-byte @(:bytes writer))) "UTF-8"))

;; ---- core.target ----------------------------------------------------------

(def architectures #{:x86-64 :aarch64 :riscv64 :rax64 :other})
(def endians #{:little :big})

(defn valid-target-info? [{:keys [pointer-bits endian architecture]}]
  (and (contains? #{32 64} pointer-bits)
       (contains? endians endian)
       (contains? architectures architecture)))

(defn pointer-bytes [target] (quot (:pointer-bits target) 8))
