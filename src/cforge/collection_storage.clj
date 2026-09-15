(ns cforge.collection-storage)

;; Bootstrap interpreter backing for Forge collection implementations.
;; This namespace deliberately exposes only raw indexed storage. Length,
;; capacity policy, growth, probing, tombstones and collection semantics live
;; in Forge source, not here.

(defn- make-provider [default-value]
  (let [next-handle (atom 1)
        stores (atom {})]
    {:create (fn [slots]
               (let [handle (swap! next-handle inc)
                     data (atom (vec (repeat (int slots) default-value)))]
                 (swap! stores assoc handle data)
                 handle))
     :slots (fn [handle]
              (count @(get @stores handle)))
     :get (fn [handle index]
            (nth @(get @stores handle) (int index)))
     :set! (fn [handle index value]
             (swap! (get @stores handle) assoc (int index) value)
             nil)
     :resize! (fn [handle new-slots]
                (let [store (get @stores handle)
                      target (int new-slots)]
                  (swap! store
                         (fn [v]
                           (cond
                             (= target (count v)) v
                             (< target (count v)) (vec (take target v))
                             :else (into v (repeat (- target (count v)) default-value)))))
                  nil))
     :swap! (fn [left right]
              (let [a @(get @stores left)
                    b @(get @stores right)]
                (reset! (get @stores left) b)
                (reset! (get @stores right) a)
                nil))}))

(def ^:dynamic *u8-storage* (make-provider 0N))
(def ^:dynamic *u32-storage* (make-provider 0N))
(def ^:dynamic *u64-storage* (make-provider 0N))
(def ^:dynamic *usize-storage* (make-provider 0N))
(def ^:dynamic *string-storage* (make-provider ""))

(defn create-u8 [slots] ((:create *u8-storage*) slots))
(defn slots-u8 [handle] ((:slots *u8-storage*) handle))
(defn get-u8 [handle index] ((:get *u8-storage*) handle index))
(defn set-u8! [handle index value] ((:set! *u8-storage*) handle index value))
(defn resize-u8! [handle slots] ((:resize! *u8-storage*) handle slots))
(defn swap-u8! [left right] ((:swap! *u8-storage*) left right))

(defn create-u32 [slots] ((:create *u32-storage*) slots))
(defn slots-u32 [handle] ((:slots *u32-storage*) handle))
(defn get-u32 [handle index] ((:get *u32-storage*) handle index))
(defn set-u32! [handle index value]
  (let [v (bigint value)]
    (when-not (<= 0N v 4294967295N)
      (throw (ex-info "raw_u32 value out of range" {:value value})))
    ((:set! *u32-storage*) handle index v)))
(defn resize-u32! [handle slots] ((:resize! *u32-storage*) handle slots))
(defn swap-u32! [left right] ((:swap! *u32-storage*) left right))

(defn create-u64 [slots] ((:create *u64-storage*) slots))
(defn slots-u64 [handle] ((:slots *u64-storage*) handle))
(defn get-u64 [handle index] ((:get *u64-storage*) handle index))
(defn set-u64! [handle index value] ((:set! *u64-storage*) handle index value))
(defn resize-u64! [handle slots] ((:resize! *u64-storage*) handle slots))
(defn swap-u64! [left right] ((:swap! *u64-storage*) left right))

(defn create-usize [slots] ((:create *usize-storage*) slots))
(defn slots-usize [handle] ((:slots *usize-storage*) handle))
(defn get-usize [handle index] ((:get *usize-storage*) handle index))
(defn set-usize! [handle index value] ((:set! *usize-storage*) handle index value))
(defn resize-usize! [handle slots] ((:resize! *usize-storage*) handle slots))
(defn swap-usize! [left right] ((:swap! *usize-storage*) left right))

(defn create-string [slots] ((:create *string-storage*) slots))
(defn slots-string [handle] ((:slots *string-storage*) handle))
(defn get-string [handle index] ((:get *string-storage*) handle index))
(defn set-string! [handle index value] ((:set! *string-storage*) handle index value))
(defn resize-string! [handle slots] ((:resize! *string-storage*) handle slots))
(defn swap-string! [left right] ((:swap! *string-storage*) left right))