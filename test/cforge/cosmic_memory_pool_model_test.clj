(ns cforge.cosmic-memory-pool-model-test
  (:require [clojure.test :refer [deftest is]]
            [cforge.cosmic-memory-pool-model :as pool]))

(deftest root-pool-accounting
  (let [root (pool/make-root-pool 1024)]
    (is (= {:limit 1024 :charged 0 :reserved 0 :available 1024 :revoked? false}
           (pool/usage root)))
    (let [{charge :ok} (pool/charge! root 256 :kernel-object)]
      (is charge)
      (is (= 768 (pool/available root)))
      (is (= 256 (:charged (pool/usage root))))
      (is (pool/valid-invariants? root))
      (is (= {:ok nil} (pool/release! root charge)))
      (is (= 1024 (pool/available root)))
      (is (pool/valid-invariants? root)))))

(deftest limit-is-hard
  (let [root (pool/make-root-pool 100)]
    (is (= :limit-exceeded (:error (pool/charge! root 101))))
    (is (:ok (pool/charge! root 100)))
    (is (= :limit-exceeded (:error (pool/charge! root 1))))
    (is (pool/valid-invariants? root))))

(deftest child-delegation-reserves-authority
  (let [root (pool/make-root-pool 1000)
        child (:ok (pool/derive! root 400))]
    (is child)
    (is (= 600 (pool/available root)))
    (is (= 400 (pool/available child)))
    (is (= :limit-exceeded (:error (pool/derive! root 601))))
    (is (:ok (pool/derive! root 600)))
    (is (= 0 (pool/available root)))
    (is (pool/valid-invariants? root))))

(deftest child-charge-does-not-double-charge-parent
  (let [root (pool/make-root-pool 1000)
        child (:ok (pool/derive! root 400))
        charge (:ok (pool/charge! child 300 :memory-object))]
    (is (= 600 (pool/available root)))
    (is (= 100 (pool/available child)))
    (is (= 300 (:charged (pool/usage child))))
    (is (pool/valid-invariants? root))
    (is (pool/valid-invariants? child))
    (is (= {:ok nil} (pool/release! child charge)))
    (is (= 400 (pool/available child)))))

(deftest nested-delegation-cannot-create-capacity
  (let [root (pool/make-root-pool 1000)
        service (:ok (pool/derive! root 600))
        worker (:ok (pool/derive! service 250))]
    (is (= 400 (pool/available root)))
    (is (= 350 (pool/available service)))
    (is (= 250 (pool/available worker)))
    (is (= :limit-exceeded (:error (pool/derive! service 351))))
    (is (pool/valid-invariants? root))
    (is (pool/valid-invariants? service))))

(deftest releasing-is-tokenized
  (let [root (pool/make-root-pool 1000)
        charge (:ok (pool/charge! root 100 :port))]
    (is (= {:ok nil} (pool/release! root charge)))
    (is (= :invalid-charge (:error (pool/release! root charge))))
    (is (= :invalid-charge
           (:error (pool/release! root (assoc charge :bytes 99)))))
    (is (pool/valid-invariants? root))))

(deftest child-destruction-requires-empty-pool
  (let [root (pool/make-root-pool 1000)
        child (:ok (pool/derive! root 300))
        charge (:ok (pool/charge! child 64 :task))]
    (is (= :pool-in-use (:error (pool/destroy-child! child))))
    (is (= {:ok nil} (pool/release! child charge)))
    (is (= {:ok nil} (pool/destroy-child! child)))
    (is (= 1000 (pool/available root)))
    (is (= :revoked (:error (pool/charge! child 1))))
    (is (pool/valid-invariants? root))))

(deftest parent-cannot-be-destroyed-before-child
  (let [root (pool/make-root-pool 1000)
        child (:ok (pool/derive! root 500))
        grandchild (:ok (pool/derive! child 200))]
    (is (= :pool-in-use (:error (pool/destroy-child! child))))
    (is (= {:ok nil} (pool/destroy-child! grandchild)))
    (is (= {:ok nil} (pool/destroy-child! child)))
    (is (= 1000 (pool/available root)))))

(deftest invalid-requests-are-rejected
  (let [root (pool/make-root-pool 100)]
    (is (= :invalid-size (:error (pool/charge! root 0))))
    (is (= :invalid-size (:error (pool/charge! root -1))))
    (is (= :invalid-size (:error (pool/derive! root 0))))))
