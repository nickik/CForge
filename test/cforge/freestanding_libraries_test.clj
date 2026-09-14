(ns cforge.freestanding-libraries-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.arena-model :as arena]
            [cforge.freestanding-model :as fs]))

(deftest mem-library-contract
  (is (fs/ranges-overlap? 0 8 4 8))
  (is (not (fs/ranges-overlap? 0 4 4 4)))
  (is (= [0 7 7 7 4 5] (fs/mem-set [0 1 2 3 4 5] 1 7 3)))
  (is (= [0 1 0 1 2 3] (fs/mem-move [0 1 2 3 4 5] 2 0 4)))
  (is (= {:error :overlap}
         (fs/mem-copy-nonoverlapping [0 1 2 3 4 5] 2 0 4)))
  (is (= [0 1 2 3 0 1 2 3]
         (:ok (fs/mem-copy-nonoverlapping [0 1 2 3 0 0 0 0] 4 0 4))))
  (is (neg? (fs/mem-compare [1 2] [1 3])))
  (is (zero? (fs/mem-compare [1 2 3] [1 2 3]))))

(deftest bits-and-endian-library-contract
  (is (= 0x3412 (fs/bswap-u16 0x1234)))
  (is (= 0x78563412 (fs/bswap-u32 0x12345678)))
  (is (= 0xff (fs/low-mask-u32 8)))
  (is (= 0xffffffff (fs/low-mask-u32 32)))
  (is (= 0x5 (fs/extract-u32 0x50 4 4)))
  (is (= 0xa0 (fs/insert-u32 0 0xa 4 4)))
  (doseq [value [0 1 0x12345678 0xffffffff]]
    (is (= value (fs/decode-u32-le (fs/encode-u32-le value))))
    (is (= value (fs/decode-u32-be (fs/encode-u32-be value))))))

(deftest mmio-library-contract
  (let [bus (fs/make-mmio {0x1000 7})]
    (is (= 7 (fs/mmio-read bus 0x1000)))
    (fs/mmio-write! bus 0x1000 42)
    (is (= 42 (fs/mmio-read bus 0x1000)))
    (is (= [[:read 0x1000] [:write 0x1000 42] [:read 0x1000]] @(:log bus)))))

(deftest atomic-library-contract
  (let [a (fs/make-atomic 10)]
    (is (= 10 (fs/atomic-load a :relaxed)))
    (is (= 10 (fs/atomic-fetch-add! a 5 :acq-rel)))
    (is (= 15 (fs/atomic-load a :acquire)))
    (is (= {:old 15 :exchanged? true}
           (fs/atomic-compare-exchange! a 15 99 :acq-rel :relaxed)))
    (is (= {:old 99 :exchanged? false}
           (fs/atomic-compare-exchange! a 15 0 :acq-rel :relaxed)))
    (is (= 99 @(:value a)))))

(deftest sync-library-contract
  (let [lock (fs/make-spin-lock)]
    (is (true? (fs/spin-try-lock! lock)))
    (is (false? (fs/spin-try-lock! lock)))
    (fs/spin-unlock! lock)
    (is (true? (fs/spin-try-lock! lock))))
  (let [events (atom [])
        section (fs/make-critical-section
                 (fn [ctx] (swap! ctx conj :enter) 77)
                 (fn [ctx token] (swap! ctx conj [:exit token]))
                 events)
        token (fs/critical-enter section)]
    (fs/critical-exit section token)
    (is (= [:enter [:exit 77]] @events))))

(deftest fixed-collections-contract
  (let [v (fs/make-fixed-vec 3)]
    (is (= {:ok nil} (fs/fixed-vec-push! v :a)))
    (is (= {:ok nil} (fs/fixed-vec-push! v :b)))
    (is (= {:ok nil} (fs/fixed-vec-push! v :c)))
    (is (= {:error :full} (fs/fixed-vec-push! v :d)))
    (is (= [:a :b :c] (fs/fixed-vec-values v)))
    (is (= {:ok :c} (fs/fixed-vec-pop! v))))
  (let [ring (fs/make-ring 3)]
    (doseq [x [1 2 3]] (is (= {:ok nil} (fs/ring-push! ring x))))
    (is (= {:error :full} (fs/ring-push! ring 4)))
    (is (= {:ok 1} (fs/ring-pop! ring)))
    (is (= {:ok nil} (fs/ring-push! ring 4)))
    (is (= [{:ok 2} {:ok 3} {:ok 4}]
           [(fs/ring-pop! ring) (fs/ring-pop! ring) (fs/ring-pop! ring)]))
    (is (= {:error :empty} (fs/ring-pop! ring)))))

(deftest intrusive-queue-contract
  (let [q (fs/make-intrusive-queue)]
    (is (= {:ok nil} (fs/intrusive-enqueue! q 101)))
    (is (= {:ok nil} (fs/intrusive-enqueue! q 202)))
    (is (= {:error :already-linked} (fs/intrusive-enqueue! q 101)))
    (is (= 2 (fs/intrusive-len q)))
    (is (= {:ok 101} (fs/intrusive-dequeue! q)))
    (is (= {:ok 202} (fs/intrusive-dequeue! q)))
    (is (= {:error :empty} (fs/intrusive-dequeue! q)))))

(deftest layout-library-contract
  (is (= {:offsets [0 8 16] :align 8 :size 24}
         (fs/layout-struct [{:size 4 :align 4}
                            {:size 8 :align 8}
                            {:size 1 :align 1}])))
  (is (= 64 (fs/align-up 33 32))))

(deftest io-library-contract
  (let [writer (fs/make-fixed-writer 32)]
    (is (= {:ok 5} (fs/writer-write-str! writer "task=")))
    (is (= {:ok 2} (fs/writer-write-u64-dec! writer 42)))
    (is (= {:ok 1} (fs/writer-write-str! writer " ")))
    (is (= {:ok 5} (fs/writer-write-u64-hex! writer 0xabc)))
    (is (= "task=42 0xabc" (fs/writer-string writer))))
  (let [writer (fs/make-fixed-writer 3)]
    (is (= {:error :full} (fs/writer-write-str! writer "four")))))

(deftest target-library-contract
  (let [target {:pointer-bits 64 :endian :little :architecture :rax64}]
    (is (fs/valid-target-info? target))
    (is (= 8 (fs/pointer-bytes target))))
  (is (not (fs/valid-target-info? {:pointer-bits 48 :endian :little :architecture :rax64}))))

(defn run-os-foundation-scenario [provider-kind]
  (let [target {:pointer-bits 64 :endian :little :architecture :rax64}
        task-layout (fs/layout-struct [{:size 8 :align 8}
                                       {:size 8 :align 8}
                                       {:size 4 :align 4}
                                       {:size 4 :align 4}])
        a (arena/make-linear-arena provider-kind (* 4 4096))
        cache (:ok (arena/make-object-cache a {:object-size (:size task-layout)
                                               :object-align (:align task-layout)
                                               :slab-size 4096}))
        runnable (fs/make-intrusive-queue)
        events (fs/make-ring 8)
        tasks-live (fs/make-atomic 0)
        lock (fs/make-spin-lock)
        bus (fs/make-mmio {0x1000 0})
        writer (fs/make-fixed-writer 128)
        addresses (mapv (fn [task-id]
                          (let [address (:ok (arena/object-cache-alloc cache :no-wait))]
                            (is (fs/spin-try-lock! lock))
                            (is (= {:ok nil} (fs/intrusive-enqueue! runnable address)))
                            (fs/spin-unlock! lock)
                            (is (= {:ok nil} (fs/ring-push! events task-id)))
                            (fs/atomic-fetch-add! tasks-live 1 :relaxed)
                            address))
                        [11 22 33])
        register (fs/insert-u32 0 (fs/atomic-load tasks-live :acquire) 8 4)
        encoded (fs/encode-u32-le register)
        memory0 (vec (concat encoded (repeat 12 0)))
        memory1 (:ok (fs/mem-copy-nonoverlapping memory0 8 0 4))]
    (is (fs/valid-target-info? target))
    (is (= 8 (fs/pointer-bytes target)))
    (fs/mmio-write! bus 0x1000 register)
    (is (= register (fs/mmio-read bus 0x1000)))
    (is (= register (fs/decode-u32-le (subvec memory1 8 12))))
    (fs/writer-write-str! writer "tasks=")
    (fs/writer-write-u64-dec! writer (fs/intrusive-len runnable))
    (fs/writer-write-str! writer " reg=")
    (fs/writer-write-u64-hex! writer register)
    (let [dequeued (mapv (fn [_] (:ok (fs/intrusive-dequeue! runnable))) (range 3))
          ring-events (mapv (fn [_] (:ok (fs/ring-pop! events))) (range 3))]
      (doseq [address addresses]
        (is (= {:ok nil} (arena/object-cache-free cache address))))
      {:task-layout task-layout
       :addresses addresses
       :dequeued dequeued
       :events ring-events
       :register register
       :diagnostic (fs/writer-string writer)
       :reclaimed (arena/object-cache-reclaim cache)
       :arena-request-count (count (:requests (arena/arena-state a)))})))

(deftest all-freestanding-libraries-compose-with-core-and-arena
  (let [kernel (run-os-foundation-scenario :kernel)
        hosted (run-os-foundation-scenario :hosted)]
    (is (= kernel hosted))
    (is (= [11 22 33] (:events kernel)))
    (is (= (:addresses kernel) (:dequeued kernel)))
    (is (= 0x300 (:register kernel)))
    (is (= "tasks=3 reg=0x300" (:diagnostic kernel)))
    (is (= 4096 (:reclaimed kernel)))
    (is (= 1 (:arena-request-count kernel)))))
