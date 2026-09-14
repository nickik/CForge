(ns cforge.application-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cforge.core :as core]))

(def game-source
  (slurp "examples/game_of_life.fg"))

(def expected-game-output
  (slurp "examples/game_of_life.expected.txt"))

(deftest while-and-mutable-assignment-execute
  (let [source "module test.loop;\nfn main() -> i32 {\n  var x: i32 = 0;\n  while (x < 5) { x += 1; }\n  return x - 5;\n}\n"
        result (core/run-source source)]
    (is (empty? (:diagnostics result)))
    (is (= 0 (:exit result)))))

(deftest immutable-assignment-is-rejected
  (let [source "module test.bad;\nfn main() -> i32 {\n  val x: i32 = 0;\n  x = 1;\n  return 0;\n}\n"
        result (core/check-source source)]
    (is (= :type/immutable
           (get-in result [:diagnostics 0 :category])))))

(deftest console-write-requires-import
  (let [source "module test.console;\nfn main() -> i32 {\n  console.write(\"x\");\n  return 0;\n}\n"
        result (core/check-source source)]
    (is (= :name/unresolved
           (get-in result [:diagnostics 0 :category])))))

(deftest console-write-works-through-host-provider
  (let [source "module test.console;\nimport std.console;\nfn main() -> i32 {\n  console.write(\"hello\\n\");\n  return 0;\n}\n"
        result (atom nil)
        output (with-out-str (reset! result (core/run-source source)))]
    (is (empty? (:diagnostics @result)))
    (is (= 0 (:exit @result)))
    (is (= "hello\n" output))))

(deftest game-of-life-is-a-real-runnable-forge-program
  (testing "parser/checker/interpreter execute four generations"
    (let [result (atom nil)
          output (with-out-str (reset! result (core/run-source game-source)))]
      (is (empty? (:diagnostics @result)))
      (is (= 0 (:exit @result)))
      (is (= expected-game-output output))
      (is (= 4 (count (re-seq #"Generation" output))))
      (is (str/includes? output ".#......\n..#.....\n###.....")))))
