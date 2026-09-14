(ns cforge.list-u64-allocator-contract-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.list-u64-model :as list]))

(deftest list-u64-grows-and-preserves-values
  (let [allocator (list/make-allocator :a 4096)]
    (loop [xs (list/create) value 0]
      (if (= value 20)
        (do
          (is (= 20 (list/len xs)))
          (is (>= (list/capacity xs) 20))
          (is (= (mapv bigint (range 20))
                 (mapv #(list/get-at xs %) (range 20)))))
        (let [result (list/push xs allocator value)]
          (is (nil? (:error result)))
          (recur (:ok result) (inc value)))))))

(deftest list-u64-insert-remove-pop-and-swap-remove
  (let [allocator (list/make-allocator :a 4096)
        a (:ok (list/push (list/create) allocator 10))
        b (:ok (list/push a allocator 20))
        c (:ok (list/push b allocator 30))
        d (:ok (list/insert c allocator 1 15))]
    (is (= [10N 15N 20N 30N]
           (mapv #(list/get-at d %) (range (list/len d)))))
    (let [{removed :value e :list} (assoc (list/remove-at d 2) :removed nil)]
      (is (= 20N removed))
      (is (= [10N 15N 30N]
             (mapv #(list/get-at e %) (range (list/len e)))))
      (let [{popped :value f :list} (list/pop e)]
        (is (= 30N popped))
        (is (= 2 (list/len f)))
        (let [{swapped :value g :list} (list/swap-remove f 0)]
          (is (= 10N swapped))
          (is (= [15N]
                 (mapv #(list/get-at g %) (range (list/len g))))))))))

(deftest oom-growth-leaves-list-unchanged
  (let [allocator (list/make-allocator :small 32)
        a (:ok (list/push (list/create) allocator 1))
        b (:ok (list/push a allocator 2))
        c (:ok (list/push b allocator 3))
        d (:ok (list/push c allocator 4))
        before d
        result (list/push d allocator 5)]
    ;; First growth allocates four u64s = 32 bytes. Growing to eight elements
    ;; requires 64 bytes and must fail without changing the original list.
    (is (= :out-of-memory (:error result)))
    (is (= before (:list result)))
    (is (= 4 (list/len (:list result))))
    (is (= 4 (list/capacity (:list result))))
    (is (= [1N 2N 3N 4N]
           (mapv #(list/get-at (:list result) %) (range 4))))))

(deftest wrong-allocator-resize-is-rejected-without-mutation
  (let [allocator-a (list/make-allocator :a 4096)
        allocator-b (list/make-allocator :b 4096)
        a (:ok (list/push (list/create) allocator-a 11))
        b (:ok (list/push a allocator-a 22))
        c (:ok (list/push b allocator-a 33))
        d (:ok (list/push c allocator-a 44))
        result (list/push d allocator-b 55)]
    (is (= :foreign-allocation (:error result)))
    (is (= d (:list result)))
    (is (= [11N 22N 33N 44N]
           (mapv #(list/get-at (:list result) %) (range 4))))
    (is (zero? (:live-bytes (list/allocator-state allocator-b))))))

(deftest wrong-allocator-destroy-is-rejected
  (let [allocator-a (list/make-allocator :a 4096)
        allocator-b (list/make-allocator :b 4096)
        xs (:ok (list/push (list/create) allocator-a 7))
        result (list/destroy xs allocator-b)]
    (is (= :foreign-allocation (:error result)))
    (is (= xs (:list result)))
    (is (= 32 (:live-bytes (list/allocator-state allocator-a))))
    (is (zero? (:live-bytes (list/allocator-state allocator-b))))))

(deftest destroy-with-owning-allocator-releases-storage
  (let [allocator (list/make-allocator :a 4096)
        xs (:ok (list/push (list/create) allocator 7))
        result (list/destroy xs allocator)]
    (is (= (list/create) (:ok result)))
    (is (zero? (:live-bytes (list/allocator-state allocator))))))
