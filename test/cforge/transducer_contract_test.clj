(ns cforge.transducer-contract-test
  (:require [clojure.test :refer [deftest is testing]]))

(defn into-vector [xf coll]
  (transduce xf conj [] coll))

(deftest stateless-transducer-contracts
  (testing "identity/map/filter/remove/keep"
    (is (= [0 1 2 3] (into-vector identity [0 1 2 3])))
    (is (= [2 4 6] (into-vector (map inc) [1 3 5])))
    (is (= [2 4] (into-vector (filter even?) [1 2 3 4 5])))
    (is (= [1 3 5] (into-vector (remove even?) [1 2 3 4 5])))
    (is (= [10 30]
           (into-vector (keep #(when (odd? %) (* % 10))) [1 2 3])))))

(deftest cat-and-mapcat-contracts
  (testing "cat emits zero, one or many values and mapcat matches map+cat"
    (is (= [1 2 3 4 5] (into-vector cat [[1 2] [] [3] [4 5]])))
    (let [f (fn [x] [x (* x 10)])]
      (is (= [1 10 2 20 3 30]
             (into-vector (mapcat f) [1 2 3])))
      (is (= (into-vector (mapcat f) [1 2 3])
             (into-vector (comp (map f) cat) [1 2 3]))))))

(deftest composition-order-contract
  (testing "composition is written in source transformation order"
    (let [xf (comp (filter even?)
                   (map #(* % 10))
                   (take 3))]
      (is (= [0 20 40] (into-vector xf (range 10)))))))

(deftest early-termination-contracts
  (testing "take stops upstream immediately"
    (let [seen (atom [])
          xf (comp (map #(do (swap! seen conj %) %))
                   (take 3))]
      (is (= [0 1 2] (into-vector xf (range 100))))
      (is (= [0 1 2] @seen))))

  (testing "take-while stops inspecting after the first failure"
    (let [seen (atom [])
          xf (take-while #(do (swap! seen conj %) (< % 3)))]
      (is (= [0 1 2] (into-vector xf (range 100))))
      (is (= [0 1 2 3] @seen))))

  (testing "take-nth"
    (is (= [0 3 6 9] (into-vector (take-nth 3) (range 10))))))

(deftest drop-contracts
  (testing "drop and drop-while"
    (is (= [3 4 5] (into-vector (drop 3) (range 6))))
    (is (= [3 2 1]
           (into-vector (drop-while #(< % 3)) [0 1 2 3 2 1])))))

(deftest indexed-contracts
  (testing "map-indexed starts at zero"
    (is (= ["0:a" "1:b" "2:c"]
           (into-vector (map-indexed #(str %1 ":" %2)) ["a" "b" "c"]))))

  (testing "keep-indexed advances index for discarded values"
    (is (= ["0:a" "2:c"]
           (into-vector (keep-indexed #(when (even? %1) (str %1 ":" %2)))
                        ["a" "b" "c" "d"])))))

(deftest dedupe-and-distinct-are-different
  (let [input [1 1 2 2 2 3 1 1 4]]
    (is (= [1 2 3 1 4] (into-vector (dedupe) input)))
    (is (= [1 2 3 4] (into-vector (distinct) input)))))

(deftest state-is-fresh-per-transduction
  (testing "stateful transducer construction can be reused safely through fresh executions"
    (let [xf (comp (dedupe) (drop 1) (map-indexed vector))
          input [1 1 2 2 3]]
      (is (= [[0 2] [1 3]] (into-vector xf input)))
      (is (= [[0 2] [1 3]] (into-vector xf input))))))

(deftest interpose-contract
  (is (= [1 0 3 0 1]
         (into-vector (comp (dedupe)
                            (filter odd?)
                            (interpose 0))
                      [1 1 2 2 2 3 1 1 4]))))

(deftest partition-all-completion-contract
  (testing "short final partition is flushed exactly once on completion"
    (is (= [[2 4] [6 8] [10]]
           (into-vector (comp (map #(* % 2))
                              (partition-all 2))
                        [1 2 3 4 5])))))

(deftest partition-by-contract
  (is (= [[1 1] [2 2 2] [3] [1 1]]
         (into-vector (partition-by identity) [1 1 2 2 2 3 1 1]))))

(deftest cross-type-pipeline-contract
  (let [parse-u64 (fn [s]
                    (when (re-matches #"[0-9]+" s)
                      (Long/parseLong s)))
        xf (comp (keep parse-u64)
                 (filter #(>= % 10))
                 (take 2))]
    (is (= [20 300]
           (into-vector xf ["1" "20" "300" "bad" "4"])))))

(deftest completion-runs-once
  (let [completed (atom 0)
        rf (fn
             ([] [])
             ([result]
              (swap! completed inc)
              result)
             ([result input]
              (conj result input)))]
    (is (= [0 1 2] (transduce (take 3) rf [] (range 10))))
    (is (= 1 @completed))))

(deftest nested-reduction-propagates-early-termination
  (let [seen (atom [])
        xf (comp cat
                 (map #(do (swap! seen conj %) %))
                 (take 3))]
    (is (= [1 2 3] (into-vector xf [[1 2] [3 4 5] [6 7]])))
    (is (= [1 2 3] @seen))))
