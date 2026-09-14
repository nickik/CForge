(ns cforge.arena-provider-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.arena-model :as arena]))

(def provider-kinds [:kernel :hosted])

(deftest arena-capability-dispatch-is-provider-independent
  (doseq [kind provider-kinds]
    (testing (name kind)
      (let [a (arena/make-linear-arena kind 16384)
            r1 (arena/arena-alloc a {:size 13 :align 8 :wait :no-wait})
            r2 (arena/arena-alloc a {:size 257 :align 64 :wait :may-wait})]
        (is (= 0 (mod (get-in r1 [:ok :address]) 8)))
        (is (= 0 (mod (get-in r2 [:ok :address]) 64)))
        (is (= [13 257] (mapv :size (:requests (arena/arena-state a)))))
        (is (= [:no-wait :may-wait]
               (mapv :wait (:requests (arena/arena-state a)))))))))

(deftest arena-exhaustion-is-result-not-panic
  (doseq [kind provider-kinds]
    (let [a (arena/make-linear-arena kind 128)]
      (is (:ok (arena/arena-alloc a {:size 96 :align 8 :wait :no-wait})))
      (is (= {:error :out-of-memory}
             (arena/arena-alloc a {:size 64 :align 8 :wait :no-wait}))))))

(deftest arbitrary-size-allocator-works-over-both-providers
  (doseq [kind provider-kinds]
    (let [a (arena/make-linear-arena kind 65536)
          alloc (arena/make-arena-allocator a)
          sizes [1 7 13 127 4096 9000]
          blocks (mapv #(arena/allocator-alloc alloc
                                               {:size % :align 16 :wait :may-wait})
                       sizes)]
      (is (every? :ok blocks))
      (is (= sizes (mapv #(get-in % [:ok :size]) blocks)))
      (doseq [result blocks]
        (arena/allocator-free alloc (:ok result)))
      (is (= (reduce + sizes) (:freed-bytes (arena/arena-state a)))))))

(deftest object-cache-uses-identical-core-algorithm-for-kernel-and-hosted
  (doseq [kind provider-kinds]
    (testing (name kind)
      (let [a (arena/make-linear-arena kind (* 4 4096))
            cache (:ok (arena/make-object-cache a {:object-size 48
                                                   :object-align 16
                                                   :slab-size 4096}))
            per-slab (arena/objects-per-slab (:spec cache))
            addresses (mapv (fn [_] (:ok (arena/object-cache-alloc cache :no-wait)))
                            (range (inc per-slab)))
            stats (arena/object-cache-stats cache)]
        (is (= 85 per-slab))
        (is (= (count addresses) (count (set addresses))))
        (is (every? #(zero? (mod % 16)) addresses))
        (is (= 2 (:slabs-total stats)))
        (is (= (inc per-slab) (:objects-in-use stats)))
        (is (= :no-wait (-> (arena/arena-state a) :requests first :wait)))))))

(deftest object-cache-free-reuse-and-invalid-free
  (doseq [kind provider-kinds]
    (let [a (arena/make-linear-arena kind 8192)
          cache (:ok (arena/make-object-cache a {:object-size 64
                                                 :object-align 64
                                                 :slab-size 4096}))
          first-address (:ok (arena/object-cache-alloc cache :may-wait))]
      (is (= {:ok nil} (arena/object-cache-free cache first-address)))
      (is (= first-address (:ok (arena/object-cache-alloc cache :may-wait))))
      (is (= {:error :invalid-free}
             (arena/object-cache-free cache (+ first-address 1)))))))

(deftest object-cache-reclaims-only-empty-slabs
  (doseq [kind provider-kinds]
    (let [a (arena/make-linear-arena kind (* 3 4096))
          cache (:ok (arena/make-object-cache a {:object-size 1024
                                                 :object-align 1024
                                                 :slab-size 4096}))
          objects (mapv (fn [_] (:ok (arena/object-cache-alloc cache :may-wait)))
                        (range 5))]
      ;; Five objects force two slabs. Keep one object in the second slab live;
      ;; free all objects from the first slab, then reclaim one slab.
      (doseq [address (take 4 objects)]
        (is (= {:ok nil} (arena/object-cache-free cache address))))
      (is (= 4096 (arena/object-cache-reclaim cache)))
      (let [stats (arena/object-cache-stats cache)]
        (is (= 1 (:slabs-total stats)))
        (is (= 1 (:objects-in-use stats)))
        (is (= 4096 (:bytes-backing stats)))))))

(deftest provider-reclaim-hook-is-dispatched
  (doseq [kind provider-kinds]
    (let [a (arena/make-linear-arena kind 4096)]
      (is (= 0 (arena/arena-reclaim a 2048)))
      (is (= [2048] (:reclaim-requests (arena/arena-state a)))))))
