(ns cforge.module-link-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.core :as core]))

(def bootstrap-library
  [["bootstrap_support" "packages/bootstrap-support/src/lib.fg"]])

(deftest imported-public-function-runs
  (let [source (str "module test.cross_package;\n"
                    "import bootstrap_support;\n"
                    "fn main() -> i32 {\n"
                    "  val value: u32 = bootstrap_support.identity_u32(42);\n"
                    "  if (value == 42) { return 0; } else { return 1; }\n"
                    "}\n")
        result (core/run-source source bootstrap-library)]
    (is (empty? (:diagnostics result)))
    (is (= 0 (:exit result)))))

(deftest imported-function-with-local-state-and-loop-runs
  (testing "cross-package calls are real calls, not one-expression inlining"
    (let [source (str "module test.cross_package_loop;\n"
                      "import bootstrap_support;\n"
                      "fn main() -> i32 {\n"
                      "  val value: u32 = bootstrap_support.count_to(7);\n"
                      "  if (value == 7) { return 0; } else { return 1; }\n"
                      "}\n")
          result (core/run-source source bootstrap-library)]
      (is (empty? (:diagnostics result)))
      (is (= 0 (:exit result))))))

(deftest local-function-call-runs
  (let [source (str "module test.local_call;\n"
                    "fn add_one(value: u32) -> u32 { return value + 1; }\n"
                    "fn main() -> i32 {\n"
                    "  val value: u32 = add_one(8);\n"
                    "  if (value == 9) { return 0; } else { return 1; }\n"
                    "}\n")
        result (core/run-source source)]
    (is (empty? (:diagnostics result)))
    (is (= 0 (:exit result)))))

(deftest missing-library-is-rejected
  (let [source (str "module test.missing;\n"
                    "import absent;\n"
                    "fn main() -> i32 { return absent.answer(1); }\n")
        result (core/check-source source bootstrap-library)]
    (is (= :link (:phase result)))
    (is (= :module/missing (get-in result [:diagnostics 0 :category])))))

(deftest wrong-module-mapping-is-rejected
  (let [source (str "module test.mismatch;\n"
                    "fn main() -> i32 { return 0; }\n")
        result (core/check-source source [["wrong_name" "packages/bootstrap-support/src/lib.fg"]])]
    (is (= :link (:phase result)))
    (is (= :module/name-mismatch (get-in result [:diagnostics 0 :category])))))

(deftest imported-function-arity-is-checked
  (let [source (str "module test.arity;\n"
                    "import bootstrap_support;\n"
                    "fn main() -> i32 {\n"
                    "  val value: u32 = bootstrap_support.identity_u32(1, 2);\n"
                    "  return 0;\n"
                    "}\n")
        result (core/check-source source bootstrap-library)]
    (is (= :link (:phase result)))
    (is (= :type/call (get-in result [:diagnostics 0 :category])))))

(deftest unexported-function-is-invisible
  (testing "only pub functions are in the dependency interface"
    (let [source (str "module test.private;\n"
                      "import bootstrap_support;\n"
                      "fn main() -> i32 {\n"
                      "  val value: u32 = bootstrap_support.private_u32(1);\n"
                      "  return 0;\n"
                      "}\n")
          result (core/check-source source bootstrap-library)]
      (is (= :link (:phase result)))
      (is (= :module/not-public (get-in result [:diagnostics 0 :category]))))))
