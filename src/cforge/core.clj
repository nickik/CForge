(ns cforge.core
  (:require [cforge.lexer :as lexer]
            [cforge.parser :as parser]
            [cforge.check :as check]
            [cforge.eval :as eval]))

(defn parse-source [source]
  (let [{:keys [tokens diagnostics]} (lexer/lex source)]
    (if (seq diagnostics)
      {:tokens tokens :ast nil :diagnostics diagnostics :phase :lex}
      (let [{:keys [ast diagnostics]} (parser/parse-tokens tokens)]
        {:tokens tokens :ast ast :diagnostics diagnostics
         :phase (if (seq diagnostics) :parse :parse)}))))

(defn check-source [source]
  (let [parsed (parse-source source)]
    (if (seq (:diagnostics parsed))
      parsed
      (let [{:keys [typed-ast diagnostics]} (check/check-file (:ast parsed))]
        (assoc parsed :typed-ast typed-ast :diagnostics diagnostics
               :phase (if (seq diagnostics) :check :check))))))

(defn run-source [source]
  (let [checked (check-source source)]
    (if (seq (:diagnostics checked))
      checked
      (let [result (eval/eval-main (:typed-ast checked))]
        (merge checked result {:phase :eval})))))
