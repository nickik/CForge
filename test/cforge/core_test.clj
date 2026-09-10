(ns cforge.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.core :as core]
            [cforge.lexer :as lexer]))

(def arithmetic-program
  "module examples.conformance.arithmetic_precedence;\n\nfn main() -> i32 {\n  val a: i32 = 2 + 3 * 4;\n  val b: i32 = (2 + 3) * 4;\n  val c: i32 = 40 / 5 + 7 % 4;\n  if (a == 14 && b == 20 && c == 11) {\n    return 0;\n  } else {\n    return 1;\n  }\n}\n")

(deftest lexer-produces-spans-and-eof
  (let [{:keys [tokens diagnostics]} (lexer/lex "module x;\n")]
    (is (empty? diagnostics))
    (is (= [:module :identifier :semicolon :eof] (mapv :kind tokens)))
    (is (= {:start 0 :end 6 :line 1 :column 1} (:span (first tokens))))
    (is (= 2 (get-in (last tokens) [:span :line])))))

(deftest parser-respects-precedence
  (let [{:keys [ast diagnostics]} (core/parse-source arithmetic-program)
        main (first (:declarations ast))
        init (get-in main [:body :statements 0 :init])]
    (is (empty? diagnostics))
    (is (= :add (:op init)))
    (is (= :mul (get-in init [:right :op])))))

(deftest arithmetic-program-runs
  (let [{:keys [exit diagnostics]} (core/run-source arithmetic-program)]
    (is (empty? diagnostics) (pr-str diagnostics))
    (is (= 0 exit))))

(deftest undefined-name-is-semantic-error
  (let [source "module test; fn main() -> i32 { return missing; }"
        result (core/check-source source)]
    (is (= :name/unresolved (get-in result [:diagnostics 0 :category])))))

(deftest mixed-bool-and-integer-is-rejected
  (let [source "module test; fn main() -> i32 { val x: i32 = true; return 0; }"
        result (core/check-source source)]
    (is (= :type/mismatch (get-in result [:diagnostics 0 :category])))))

(deftest overflow-is-detected-during-checking
  (let [source "module test; fn main() -> i32 { val x: i8 = 128; return 0; }"
        result (core/check-source source)]
    (is (= :type/overflow (get-in result [:diagnostics 0 :category])))))
