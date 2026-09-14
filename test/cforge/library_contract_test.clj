(ns cforge.library-contract-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def core-source (slurp "lib/core.fg"))
(def std-source (slurp "lib/std.fg"))

(defn power-of-two? [n]
  (and (pos? n) (zero? (bit-and n (dec n)))))

(defn align-up [value align]
  (bit-and (+ value (dec align)) (bit-not (dec align))))

(defn valid-object-cache-spec? [{:keys [object-size object-align slab-size]}]
  (and (pos? object-size)
       (power-of-two? object-align)
       (power-of-two? slab-size)
       (>= slab-size object-size)
       (<= (align-up object-size object-align) slab-size)))

(deftest core-is-freestanding
  (testing "core never imports hosted std"
    (is (not (re-find #"(?m)^\s*import\s+std(?:\.|;)" core-source))))
  (testing "std layers on core"
    (is (re-find #"(?m)^\s*import\s+core\s*;" std-source))))

(deftest core-defines-panic-contract-without-policy
  (doseq [required ["PanicKind" "PanicLocation" "PanicInfo"
                    "IntegerOverflow" "DivideByZero" "Bounds"
                    "InvalidShift" "AllocationFailure"]]
    (is (str/includes? core-source required) required))
  (testing "core documents the one non-returning environment hook"
    (is (str/includes? core-source "__forge_panic"))
    (is (str/includes? core-source "-> never")))
  (testing "core does not pretend to provide hosted termination"
    (is (not (re-find #"(?i)exit\s*\(" core-source)))
    (is (not (re-find #"(?i)abort\s*\(" core-source)))))

(deftest arena-is-explicit-capability-not-language-interface
  (doseq [required ["ArenaOps" "context: *void" "ops: &ArenaOps"
                    "arena_alloc" "arena_free" "arena_reclaim"]]
    (is (str/includes? core-source required) required))
  (testing "dispatch is explicit through the operations table"
    (is (str/includes? core-source "arena.ops.alloc(arena.context, request)"))
    (is (str/includes? core-source "arena.ops.free(arena.context, block)"))
    (is (str/includes? core-source "arena.ops.reclaim(arena.context, target_bytes)"))))

(deftest allocator-is-also-explicit-capability
  (doseq [required ["AllocatorOps" "ops: &AllocatorOps"
                    "allocator_alloc" "allocator_free" "allocator_resize"]]
    (is (str/includes? core-source required) required))
  (is (str/includes? core-source "allocator.ops.alloc(allocator.context, request)")))

(deftest allocation-failure-is-not-implicit-panic
  (is (str/includes? core-source "Result[MemoryBlock, AllocError]"))
  (is (str/includes? core-source "Primitive allocation never implicitly panics")))

(deftest arbitrary-size-and-object-cache-allocation-both-exist
  (doseq [required ["AllocRequest" "MemoryBlock" "Allocator"
                    "allocator_alloc" "allocator_free" "allocator_resize"
                    "ObjectCacheSpec" "ObjectCache" "object_cache_alloc"
                    "object_cache_free" "object_cache_reclaim"]]
    (is (str/includes? core-source required) required)))

(deftest power-of-two-contract
  (is (false? (power-of-two? 0)))
  (is (true? (power-of-two? 1)))
  (is (true? (power-of-two? 2)))
  (is (true? (power-of-two? 4096)))
  (is (false? (power-of-two? 3)))
  (is (false? (power-of-two? 4095))))

(deftest align-up-contract
  (is (= 0 (align-up 0 8)))
  (is (= 8 (align-up 1 8)))
  (is (= 8 (align-up 8 8)))
  (is (= 16 (align-up 9 8)))
  (is (= 64 (align-up 33 32))))

(deftest object-cache-spec-validation
  (is (valid-object-cache-spec? {:object-size 24 :object-align 8 :slab-size 4096}))
  (is (valid-object-cache-spec? {:object-size 4096 :object-align 4096 :slab-size 4096}))
  (is (not (valid-object-cache-spec? {:object-size 0 :object-align 8 :slab-size 4096})))
  (is (not (valid-object-cache-spec? {:object-size 24 :object-align 3 :slab-size 4096})))
  (is (not (valid-object-cache-spec? {:object-size 8192 :object-align 8 :slab-size 4096})))
  (is (not (valid-object-cache-spec? {:object-size 24 :object-align 8 :slab-size 3000}))))

(deftest object-cache-capacity-examples
  (let [stride (align-up 24 8)]
    (is (= 24 stride))
    (is (= 170 (quot 4096 stride))))
  (let [stride (align-up 33 16)]
    (is (= 48 stride))
    (is (= 85 (quot 4096 stride)))))
