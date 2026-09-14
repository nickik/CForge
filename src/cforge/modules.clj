(ns cforge.modules
  (:require [clojure.string :as str]
            [cforge.lexer :as lexer]
            [cforge.parser :as parser]))

(defn- diagnostic [category message span]
  {:category category :severity :error :message message :span span})

(defn- parse-file [path]
  (let [{:keys [tokens diagnostics]} (lexer/lex (slurp path))]
    (when (seq diagnostics)
      (throw (ex-info "library lex failed" {:diagnostic (first diagnostics)})))
    (let [{:keys [ast diagnostics]} (parser/parse-tokens tokens)]
      (when (seq diagnostics)
        (throw (ex-info "library parse failed" {:diagnostic (first diagnostics)})))
      ast)))

(defn- normalize-library-name [name]
  (str/replace name "-" "_"))

(defn- module-name [ast]
  (str/join "." (get-in ast [:module :name])))

(defn- public-functions [ast]
  (into {}
        (for [decl (:declarations ast)
              :when (and (= :function-decl (:node decl)) (:public? decl))]
          [(:name decl) decl])))

(defn load-libraries
  "Load NAME=PATH library roots and return module metadata keyed by source module name."
  [library-specs]
  (reduce
   (fn [libraries [logical-name path]]
     (let [ast (parse-file path)
           source-name (module-name ast)
           expected (normalize-library-name logical-name)]
       (when-not (= expected source-name)
         (throw (ex-info "library/module name mismatch"
                         {:diagnostic (diagnostic :module/name-mismatch
                                                  (str "library " logical-name
                                                       " provides module " source-name
                                                       ", expected " expected)
                                                  (:span ast))})))
       (when (contains? libraries source-name)
         (throw (ex-info "duplicate library module"
                         {:diagnostic (diagnostic :module/duplicate
                                                  (str "duplicate module " source-name)
                                                  (:span ast))})))
       (assoc libraries source-name
              {:logical-name logical-name
               :path path
               :ast ast
               :functions (public-functions ast)})))
   {}
   library-specs))

(declare rewrite-expr rewrite-statement)

(defn- imported-call [expr imported-modules]
  (let [callee (:callee expr)]
    (when (and (= :member (:node callee))
               (= :name (get-in callee [:target :node])))
      (let [module (get-in callee [:target :name])]
        (when (contains? imported-modules module)
          [module (:member callee)])))))

(defn- rewrite-imported-call [expr libraries imported-modules]
  (when-let [[module function-name] (imported-call expr imported-modules)]
    (let [library (get libraries module)
          function (get-in library [:functions function-name])]
      (when-not library
        (throw (ex-info "missing imported library"
                        {:diagnostic (diagnostic :module/missing
                                                 (str "no library supplied for import " module)
                                                 (:span expr))})))
      (when-not function
        (throw (ex-info "symbol is not exported"
                        {:diagnostic (diagnostic :module/not-public
                                                 (str module "." function-name
                                                      " is missing or not public")
                                                 (:span expr))})))
      (when-not (= (count (:params function)) (count (:args expr)))
        (throw (ex-info "imported function arity mismatch"
                        {:diagnostic (diagnostic :type/call
                                                 (str module "." function-name
                                                      " expects " (count (:params function))
                                                      " arguments, found " (count (:args expr)))
                                                 (:span expr))})))
      (assoc expr
             :callee {:node :name
                      :name (str module "." function-name)
                      :span (get-in expr [:callee :span])}
             :args (mapv #(rewrite-expr % libraries imported-modules) (:args expr))))))

(defn rewrite-expr [expr libraries imported-modules]
  (if-not (map? expr)
    expr
    (or (when (= :call (:node expr))
          (rewrite-imported-call expr libraries imported-modules))
        (case (:node expr)
          :call (assoc expr
                       :callee (rewrite-expr (:callee expr) libraries imported-modules)
                       :args (mapv #(rewrite-expr % libraries imported-modules) (:args expr)))
          :binary (assoc expr
                         :left (rewrite-expr (:left expr) libraries imported-modules)
                         :right (rewrite-expr (:right expr) libraries imported-modules))
          :unary (assoc expr :expr (rewrite-expr (:expr expr) libraries imported-modules))
          :member (assoc expr :target (rewrite-expr (:target expr) libraries imported-modules))
          :index (assoc expr
                        :target (rewrite-expr (:target expr) libraries imported-modules)
                        :index (rewrite-expr (:index expr) libraries imported-modules))
          expr))))

(defn- rewrite-block [block libraries imported-modules]
  (assoc block :statements
         (mapv #(rewrite-statement % libraries imported-modules) (:statements block))))

(defn rewrite-statement [stmt libraries imported-modules]
  (case (:node stmt)
    :value-decl (assoc stmt :init (rewrite-expr (:init stmt) libraries imported-modules))
    :assignment (assoc stmt
                       :target (rewrite-expr (:target stmt) libraries imported-modules)
                       :value (rewrite-expr (:value stmt) libraries imported-modules))
    :return (cond-> stmt
              (:expr stmt) (assoc :expr (rewrite-expr (:expr stmt) libraries imported-modules)))
    :expression-statement (assoc stmt :expr (rewrite-expr (:expr stmt) libraries imported-modules))
    :while (assoc stmt
                  :condition (rewrite-expr (:condition stmt) libraries imported-modules)
                  :body (rewrite-block (:body stmt) libraries imported-modules))
    :if (assoc stmt
               :condition (rewrite-expr (:condition stmt) libraries imported-modules)
               :then (rewrite-block (:then stmt) libraries imported-modules)
               :else (when-let [else (:else stmt)]
                       (if (= :block (:node else))
                         (rewrite-block else libraries imported-modules)
                         (rewrite-statement else libraries imported-modules))))
    :block (rewrite-block stmt libraries imported-modules)
    stmt))

(defn- qualified-public-declarations [module library]
  (mapv (fn [[name function]]
          (-> function
              (assoc :name (str module "." name)
                     :linked-module module)
              (update :body #(rewrite-block % {} #{}))))
        (:functions library)))

(defn link-root
  "Resolve root imports against supplied libraries and link exported dependency
   functions into the root compilation unit. Calls remain real calls; they are no
   longer expression-inlined by the bootstrap interpreter."
  [root-ast libraries]
  (let [root-imports (:imports root-ast)
        import-names (set (mapcat :names root-imports))
        imported-modules (set (for [segments import-names
                                    :when (= 1 (count segments))]
                                (first segments)))]
    (doseq [module imported-modules]
      (when-not (or (contains? libraries module)
                    (= module "core")
                    (= module "std"))
        (throw (ex-info "unresolved imported module"
                        {:diagnostic (diagnostic :module/missing
                                                 (str "no library supplied for import " module)
                                                 (:span root-ast))}))))
    (let [linked-modules (filter #(contains? libraries %) imported-modules)
          library-imports (mapcat #(get-in libraries [% :ast :imports]) linked-modules)
          linked-decls (mapcat #(qualified-public-declarations % (get libraries %)) linked-modules)
          rewritten-root (mapv (fn [decl]
                                 (if (= :function-decl (:node decl))
                                   (assoc decl :body (rewrite-block (:body decl) libraries imported-modules))
                                   decl))
                               (:declarations root-ast))]
      (assoc root-ast
             :imports (vec (concat root-imports library-imports))
             :declarations (vec (concat rewritten-root linked-decls))))))
