(ns cforge.host-services-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.host-services :as host]))

(deftest args-provider-is-explicit-and-bounds-checked
  (binding [host/*program-args* ["a" "b"]]
    (is (= 2 (host/args-count)))
    (is (= "a" (host/args-get 0)))
    (is (= "b" (host/args-get 1)))
    (is (thrown? clojure.lang.ExceptionInfo (host/args-get 2)))))

(deftest file-provider-is-replaceable
  (let [state (atom {})
        provider {:exists? #(contains? @state %)
                  :read-text #(get @state % "")
                  :write-text (fn [path text] (swap! state assoc path text) nil)
                  :append-text (fn [path text] (swap! state update path (fnil str "") text) nil)}]
    (binding [host/*file-provider* provider]
      (is (false? (host/file-exists? "db")))
      (is (= "" (host/file-read-text "db")))
      (host/file-write-text! "db" "a")
      (host/file-append-text! "db" "b")
      (is (host/file-exists? "db"))
      (is (= "ab" (host/file-read-text "db"))))))

(deftest clock-provider-is-replaceable
  (let [ticks (atom 10)]
    (binding [host/*clock-provider* {:monotonic-us #(swap! ticks + 5)}]
      (is (= 15 (host/monotonic-us)))
      (is (= 20 (host/monotonic-us))))))

(deftest lock-provider-releases-even-on-failure
  (let [events (atom [])
        provider {:acquire-exclusive (fn [path] (swap! events conj [:acquire path]) :token)
                  :release (fn [token] (swap! events conj [:release token]) nil)}]
    (binding [host/*lock-provider* provider]
      (is (thrown? RuntimeException
                   (host/with-exclusive-file-lock "db" #(throw (RuntimeException. "boom")))))
      (is (= [[:acquire "db"] [:release :token]] @events)))))

(deftest map-provider-basic-semantics
  (let [m (host/map-create)]
    (is (= 0 (host/map-count m)))
    (is (false? (host/map-contains? m "x")))
    (host/map-put! m "x" "1")
    (is (host/map-contains? m "x"))
    (is (= "1" (host/map-get m "x")))
    (host/map-put! m "x" "2")
    (is (= "2" (host/map-get m "x")))
    (is (= 1 (host/map-count m)))))
