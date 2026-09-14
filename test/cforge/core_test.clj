(ns cforge.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.core :as core]
            [cforge.lexer :as lexer]))

(def arithmetic-program
  "module examples.conformance.arithmetic_precedence;\n\nfn main() -> i32 {\n  val a: i32 = 2 + 3 * 4;\n  val b: i32 = (2 + 3) * 4;\n  val c: i32 = 40 / 5 + 7 % 4;\n  if (a == 14 && b == 20 && c == 11) {\n    return 0;\n  } else {\n    return 1;\n  }\n}\n")

(defn diagnostic-category [source phase]
  (get-in ((case phase
             :parse core/parse-source
             :check core/check-source
             :run core/run-source)
           source)
          [:diagnostics 0 :category]))

(deftest lexer-produces-spans-and-eof
  (let [{:keys [tokens diagnostics]} (lexer/lex "module x;\n")]
    (is (empty? diagnostics))
    (is (= [:module :identifier :semicolon :eof] (mapv :kind tokens)))
    (is (= {:start 0 :end 6 :line 1 :column 1} (:span (first tokens))))
    (is (= 2 (get-in (last tokens) [:span :line])))))

(deftest lexer-handles-line-comments
  (let [{:keys [tokens diagnostics]} (lexer/lex "module x; // comment\nfn main() -> i32 { return 0; }")]
    (is (empty? diagnostics))
    (is (= :module (:kind (first tokens))))
    (is (= :eof (:kind (last tokens))))))

(deftest lexer-handles-block-comments-and-newlines
  (let [{:keys [tokens diagnostics]} (lexer/lex "module x; /* a\n b */ fn main() -> i32 { return 0; }")]
    (is (empty? diagnostics))
    (is (= 2 (get-in (first (filter #(= :fn (:kind %)) tokens)) [:span :line])))))

(deftest lexer-rejects-unterminated-block-comment
  (is (= :lex/unterminated-comment
         (get-in (lexer/lex "module x; /* never closes") [:diagnostics 0 :category]))))

(deftest lexer-handles-string-escapes
  (let [{:keys [tokens diagnostics]} (lexer/lex "\"a\\n\\t\\\"\\\\b\"")
        t (first tokens)]
    (is (empty? diagnostics))
    (is (= :string (:kind t)))
    (is (= "a\n\t\"\\b" (:value t)))))

(deftest lexer-rejects-invalid-string-escape
  (is (= :lex/invalid-string
         (get-in (lexer/lex "\"\\q\"") [:diagnostics 0 :category]))))

(deftest parser-respects-precedence
  (let [{:keys [ast diagnostics]} (core/parse-source arithmetic-program)
        main (first (:declarations ast))
        init (get-in main [:body :statements 0 :init])]
    (is (empty? diagnostics))
    (is (= :add (:op init)))
    (is (= :mul (get-in init [:right :op])))))

(deftest parser-parentheses-override-precedence
  (let [source "module test; fn main() -> i32 { return (2 + 3) * 4; }"
        ast (:ast (core/parse-source source))
        expr (get-in ast [:declarations 0 :body :statements 0 :expr])]
    (is (= :mul (:op expr)))
    (is (= :add (get-in expr [:left :op])))))

(deftest parser-logical-precedence
  (let [source "module test; fn main() -> i32 { if (true || false && false) { return 0; } else { return 1; } }"
        ast (:ast (core/parse-source source))
        expr (get-in ast [:declarations 0 :body :statements 0 :condition])]
    (is (= :logical-or (:op expr)))
    (is (= :logical-and (get-in expr [:right :op])))))

(deftest parser-parses-qualified-modules-and-imports
  (let [{:keys [ast diagnostics]} (core/parse-source "module alpha.beta; import std.io, std.mem; fn main() -> i32 { return 0; }")]
    (is (empty? diagnostics))
    (is (= ["alpha" "beta"] (get-in ast [:module :name])))
    (is (= [["std" "io"] ["std" "mem"]] (get-in ast [:imports 0 :names])))))

(deftest arithmetic-program-runs
  (let [{:keys [exit diagnostics]} (core/run-source arithmetic-program)]
    (is (empty? diagnostics) (pr-str diagnostics))
    (is (= 0 exit))))

(deftest all-bootstrap-integer-boundaries-are-accepted
  (doseq [[t lo hi] [[:i8 -128 127] [:u8 0 255]
                     [:i16 -32768 32767] [:u16 0 65535]
                     [:i32 -2147483648 2147483647] [:u32 0 4294967295]
                     [:i64 -9223372036854775808 9223372036854775807]
                     [:u64 0 18446744073709551615]]]
    (testing (name t)
      (let [source (str "module test; fn main() -> i32 { val lo: " (name t) " = " lo
                        "; val hi: " (name t) " = " hi "; return 0; }")
            result (core/check-source source)]
        (is (empty? (:diagnostics result)) (pr-str (:diagnostics result)))))))

(deftest integer-literal-overflow-is-rejected
  (doseq [[type value] [["i8" "128"] ["i8" "-129"] ["u8" "256"]
                        ["u8" "-1"] ["i16" "32768"] ["u16" "65536"]]]
    (testing (str type " " value)
      (is (= :type/overflow
             (diagnostic-category
              (str "module test; fn main() -> i32 { val x: " type " = " value "; return 0; }")
              :check))))))

(deftest unary-arithmetic-runs
  (let [{:keys [exit diagnostics]}
        (core/run-source "module test; fn main() -> i32 { val x: i32 = -5; val y: i32 = +x; if (y == -5) { return 0; } else { return 1; } }")]
    (is (empty? diagnostics) (pr-str diagnostics))
    (is (= 0 exit))))

(deftest comparisons-run
  (let [{:keys [exit diagnostics]}
        (core/run-source "module test; fn main() -> i32 { if (1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3 && 4 == 4 && 4 != 5) { return 0; } else { return 1; } }")]
    (is (empty? diagnostics) (pr-str diagnostics))
    (is (= 0 exit))))

(deftest boolean-equality-is-valid
  (let [{:keys [exit diagnostics]}
        (core/run-source "module test; fn main() -> i32 { if (true == true && false != true) { return 0; } else { return 1; } }")]
    (is (empty? diagnostics) (pr-str diagnostics))
    (is (= 0 exit))))

(deftest boolean-ordering-is-rejected
  (is (= :type/mismatch
         (diagnostic-category
          "module test; fn main() -> i32 { if (true < false) { return 1; } else { return 0; } }"
          :check))))

(deftest logical-operators-short-circuit
  (doseq [source ["module test; fn main() -> i32 { if (false && (1 / 0 == 0)) { return 1; } else { return 0; } }"
                  "module test; fn main() -> i32 { if (true || (1 / 0 == 0)) { return 0; } else { return 1; } }"]]
    (let [{:keys [exit diagnostics]} (core/run-source source)]
      (is (empty? diagnostics) (pr-str diagnostics))
      (is (= 0 exit)))))

(deftest divide-and-remainder-by-zero-trap
  (doseq [op ["/" "%"]]
    (is (= :runtime/divide-by-zero
           (diagnostic-category
            (str "module test; fn main() -> i32 { val x: i32 = 4 " op " 0; return x; }")
            :run)))))

(deftest checked-arithmetic-overflow-traps
  (doseq [[type expr] [["i8" "127 + 1"] ["u8" "255 + 1"] ["i8" "-128 - 1"]]]
    (is (= :runtime/overflow
           (diagnostic-category
            (str "module test; fn main() -> i32 { val x: " type " = " expr "; return 0; }")
            :run)))))

(deftest bitwise-operations-run
  (let [{:keys [exit diagnostics]}
        (core/run-source "module test; fn main() -> i32 { val a: i32 = 6 & 3; val b: i32 = 4 | 1; val c: i32 = 7 ^ 3; if (a == 2 && b == 5 && c == 4) { return 0; } else { return 1; } }")]
    (is (empty? diagnostics) (pr-str diagnostics))
    (is (= 0 exit))))

(deftest undefined-name-is-semantic-error
  (is (= :name/unresolved
         (diagnostic-category "module test; fn main() -> i32 { return missing; }" :check))))

(deftest mixed-bool-and-integer-is-rejected
  (is (= :type/mismatch
         (diagnostic-category "module test; fn main() -> i32 { val x: i32 = true; return 0; }" :check))))

(deftest concrete-integer-widths-do-not-mix
  (is (= :type/mismatch
         (diagnostic-category
          "module test; fn main() -> i32 { val a: u8 = 1; val b: u32 = 2; val c: u32 = a + b; return 0; }"
          :check))))

(deftest duplicate-top-level-declarations-are-rejected
  (is (= :name/duplicate
         (diagnostic-category
          "module test; fn main() -> i32 { return 0; } fn main() -> i32 { return 0; }"
          :check))))

(deftest missing-return-is-rejected
  (is (= :type/return
         (diagnostic-category "module test; fn main() -> i32 { val x: i32 = 1; }" :check))))

(deftest return-on-only-one-if-branch-is-rejected
  (is (= :type/return
         (diagnostic-category
          "module test; fn main() -> i32 { if (true) { return 0; } }"
          :check))))

(deftest return-on-both-if-branches-is-accepted
  (let [result (core/check-source
                "module test; fn main() -> i32 { if (true) { return 0; } else { return 1; } }")]
    (is (empty? (:diagnostics result)) (pr-str (:diagnostics result)))))

(deftest main-must-return-i32
  (is (= :type/main
         (diagnostic-category "module test; fn main() -> bool { return true; }" :check))))

(deftest main-must-have-no-parameters-in-bootstrap
  (is (= :type/main
         (diagnostic-category "module test; fn main(x: i32) -> i32 { return x; }" :check))))

(deftest main-is-required-for-execution
  (is (= :name/main-missing
         (diagnostic-category "module test; fn helper() -> i32 { return 0; }" :run))))

(deftest unsupported-top-level-syntax-is-explicit
  (is (= :parse/unsupported-syntax
         (diagnostic-category "module test; struct Point { x: i32; }" :parse))))

(deftest calls-parse-but-are-not-silently-executed
  (is (= :type/unsupported
         (diagnostic-category
          "module test; fn helper() -> i32 { return 1; } fn main() -> i32 { return helper(); }"
          :check))))
