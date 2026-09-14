(ns cforge.ckv-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [cforge.ckv :as ckv]
            [cforge.host-services :as host])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-db []
  (let [dir (Files/createTempDirectory "cforge-ckv-" (make-array FileAttribute 0))]
    (str (.resolve dir "test.db"))))

(deftest set-and-get-persist-through-real-file
  (let [path (temp-db)
        set-result (ckv/run-command ["--database-file" path "set" "test" "5"])
        get-result (ckv/run-command ["--database-file" path "get" "test"])]
    (is (= 0 (:exit set-result)))
    (is (re-find #"access: [0-9]+ us" (:stdout set-result)))
    (is (= 0 (:exit get-result)))
    (is (re-find #"^5\naccess: [0-9]+ us\n$" (:stdout get-result)))
    (is (= "test\t5\n" (slurp path)))))

(deftest latest-value-wins-after-full-reload
  (let [path (temp-db)]
    (ckv/set-value! path "key" "one")
    (ckv/set-value! path "other" "x")
    (ckv/set-value! path "key" "two")
    (let [result (ckv/get-value path "key")]
      (is (:found? result))
      (is (= "two" (:value result))))
    (is (= 3 (count (line-seq (io/reader path)))))))

(deftest missing-key-is-distinct-from-empty-value
  (let [path (temp-db)]
    (ckv/set-value! path "empty" "")
    (is (= "" (:value (ckv/get-value path "empty"))))
    (let [missing (ckv/run-command ["--database-file" path "get" "missing"])]
      (is (= 2 (:exit missing)))
      (is (= "key not found\n" (:stderr missing))))))

(deftest every-operation-reloads-entire-file
  (let [reads (atom 0)
        state (atom {"db" "a\t1\nb\t2\n"})
        provider {:exists? #(contains? @state %)
                  :read-text (fn [path] (swap! reads inc) (get @state path ""))
                  :write-text (fn [path text] (swap! state assoc path text) nil)
                  :append-text (fn [path text] (swap! state update path (fnil str "") text) nil)}
        lock-provider {:acquire-exclusive (fn [_] :token)
                       :release (fn [_] nil)}]
    (binding [host/*file-provider* provider
              host/*lock-provider* lock-provider]
      (is (= "1" (:value (ckv/get-value "db" "a"))))
      (ckv/set-value! "db" "c" "3")
      (is (= 2 @reads))
      (is (= "a\t1\nb\t2\nc\t3\n" (@state "db"))))))

(deftest operation-is-locked-and-timed
  (let [events (atom [])
        ticks (atom 100)
        lock-provider {:acquire-exclusive (fn [path] (swap! events conj [:lock path]) :token)
                       :release (fn [token] (swap! events conj [:unlock token]) nil)}
        file-provider {:exists? (constantly true)
                       :read-text (fn [path] (swap! events conj [:read path]) "x\t7\n")
                       :write-text (fn [& _] nil)
                       :append-text (fn [& _] nil)}
        clock-provider {:monotonic-us #(let [v @ticks] (swap! ticks + 25) v)}]
    (binding [host/*lock-provider* lock-provider
              host/*file-provider* file-provider
              host/*clock-provider* clock-provider]
      (let [result (ckv/get-value "db" "x")]
        (is (= 25 (:elapsed-us result)))
        (is (= [[:lock "db"] [:read "db"] [:unlock :token]] @events))))))

(deftest malformed-database-is-rejected
  (let [provider {:exists? (constantly true)
                  :read-text (constantly "not-a-record\n")
                  :write-text (fn [& _] nil)
                  :append-text (fn [& _] nil)}]
    (binding [host/*file-provider* provider]
      (try
        (ckv/load-database "db")
        (is false "expected corrupt database diagnostic")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ckv/corrupt (get-in (ex-data e) [:diagnostic :category]))))))))

(deftest tabs-and-newlines-are-rejected-for-now
  (let [path (temp-db)]
    (is (= :ckv/invalid-field
           (:category (:diagnostic (ckv/run-command ["--database-file" path "set" "bad\tkey" "v"])))))
    (is (= :ckv/invalid-field
           (:category (:diagnostic (ckv/run-command ["--database-file" path "set" "key" "bad\nvalue"])))))))
