(ns cforge.core
  (:require [cforge.lexer :as lexer]
            [cforge.parser :as parser]
            [cforge.check :as check]
            [cforge.eval :as eval]
            [cforge.modules :as modules]))

(defn parse-source [source]
  (let [{:keys [tokens diagnostics]} (lexer/lex source)]
    (if (seq diagnostics)
      {:tokens tokens :ast nil :diagnostics diagnostics :phase :lex}
      (let [{:keys [ast diagnostics]} (parser/parse-tokens tokens)]
        {:tokens tokens :ast ast :diagnostics diagnostics
         :phase :parse}))))

(defn- link-parsed [parsed library-specs]
  (if (or (seq (:diagnostics parsed)) (empty? library-specs))
    parsed
    (try
      (let [libraries (modules/load-libraries library-specs)
            linked (modules/link-root (:ast parsed) libraries)]
        (assoc parsed :ast linked :libraries libraries :phase :link))
      (catch clojure.lang.ExceptionInfo e
        (assoc parsed :ast nil :phase :link
               :diagnostics [(or (:diagnostic (ex-data e))
                                 {:category :module/internal
                                  :severity :error
                                  :message (.getMessage e)})])))))

(defn check-source
  ([source] (check-source source []))
  ([source library-specs]
   (let [parsed (link-parsed (parse-source source) library-specs)]
     (if (seq (:diagnostics parsed))
       parsed
       (let [{:keys [typed-ast diagnostics]} (check/check-file (:ast parsed))]
         (assoc parsed :typed-ast typed-ast :diagnostics diagnostics
                :phase :check))))))

(defn run-source
  ([source] (run-source source []))
  ([source library-specs]
   (let [checked (check-source source library-specs)]
     (if (seq (:diagnostics checked))
       checked
       (let [result (eval/eval-main (:typed-ast checked))]
         (merge checked result {:phase :eval}))))))