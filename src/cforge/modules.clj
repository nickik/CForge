(ns cforge.modules
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
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
  "Load NAME=PATH library roots and return module metadata keyed by the source module name.
   Package names may contain '-' while Forge module identifiers use '_'."
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

(defn- simple-return-expression [function]
  (let [statements (get-in function [:body :statements])]
    (when (and (= 1 (count statements))
               (= :return (:node (first statements)))
               (some? (:expr (first statements))))
      (:expr (first statements)))))

(defn- substitute-params [expr params args]
  (let [bindings (zipmap (map :name params) args)]
    (walk/postwalk
     (fn [node]
       (if (and (map? node)
                (= :name (:node node))
                (contains? bindings (:name node)))
         (get bindings (:name node))
         node))
     expr)))

(declare expand-expr)

(defn- imported-call [expr libraries imported-modules]
  (let [callee (:callee expr)]
    (when (and (= :member (:node callee))
               (= :name (get-in callee [:target :node])))
      (let [module (get-in callee [:target :name])
            function (:member callee)]
        (when (contains? imported-modules module)
          [module function])))))

(defn- expand-imported-call [expr libraries imported-modules depth]
  (when (> depth 64)
    (throw (ex-info "import expansion depth exceeded"
                    {:diagnostic (diagnostic :module/expansion-depth
                                             "imported function expansion exceeded 64 calls"
                                             (:span expr))})))
  (if-let [[module function-name] (imported-call expr libraries imported-modules)]
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
      (let [body-expr (simple-return-expression function)]
        (when-not body-expr
          (throw (ex-info "bootstrap imported function is not inlineable"
                          {:diagnostic (diagnostic :module/bootstrap-call
                                                   (str module "." function-name
                                                        " must currently consist of one return expression")
                                                   (:span function))})))
        (let [expanded-args (mapv #(expand-expr % libraries imported-modules (inc depth))
                                  (:args expr))
              substituted (substitute-params body-expr (:params function) expanded-args)]
          (expand-expr (assoc substituted :span (:span expr))
                       libraries imported-modules (inc depth)))))
    nil))

(defn expand-expr [expr libraries imported-modules depth]
  (if-not (map? expr)
    expr
    (or (when (= :call (:node expr))
          (expand-imported-call expr libraries imported-modules depth))
        (case (:node expr)
          :call (assoc expr :callee (expand-expr (:callee expr) libraries imported-modules depth)
                            :args (mapv #(expand-expr % libraries imported-modules depth) (:args expr)))
          :binary (assoc expr
                         :left (expand-expr (:left expr) libraries imported-modules depth)
                         :right (expand-expr (:right expr) libraries imported-modules depth))
          :unary (assoc expr :expr (expand-expr (:expr expr) libraries imported-modules depth))
          :member (assoc expr :target (expand-expr (:target expr) libraries imported-modules depth))
          :index (assoc expr
                        :target (expand-expr (:target expr) libraries imported-modules depth)
                        :index (expand-expr (:index expr) libraries imported-modules depth))
          expr))))

(declare expand-statement)

(defn- expand-block [block libraries imported-modules]
  (assoc block :statements
         (mapv #(expand-statement % libraries imported-modules) (:statements block))))

(defn expand-statement [stmt libraries imported-modules]
  (case (:node stmt)
    :value-decl (assoc stmt :init (expand-expr (:init stmt) libraries imported-modules 0))
    :assignment (assoc stmt :target (expand-expr (:target stmt) libraries imported-modules 0)
                            :value (expand-expr (:value stmt) libraries imported-modules 0))
    :return (cond-> stmt (:expr stmt) (assoc :expr (expand-expr (:expr stmt) libraries imported-modules 0)))
    :expression-statement (assoc stmt :expr (expand-expr (:expr stmt) libraries imported-modules 0))
    :while (assoc stmt
                  :condition (expand-expr (:condition stmt) libraries imported-modules 0)
                  :body (expand-block (:body stmt) libraries imported-modules))
    :if (assoc stmt
               :condition (expand-expr (:condition stmt) libraries imported-modules 0)
               :then (expand-block (:then stmt) libraries imported-modules)
               :else (when-let [else (:else stmt)]
                       (if (= :block (:node else))
                         (expand-block else libraries imported-modules)
                         (expand-statement else libraries imported-modules))))
    :block (expand-block stmt libraries imported-modules)
    stmt))

(defn link-root
  "Resolve imports against supplied libraries and lower currently executable imported calls.
   std.console remains a platform-provided standard-library import."
  [root-ast libraries]
  (let [imports (set (mapcat :names (:imports root-ast)))
        imported-modules (set (for [segments imports
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
    (assoc root-ast :declarations
           (mapv (fn [decl]
                   (if (= :function-decl (:node decl))
                     (assoc decl :body (expand-block (:body decl) libraries imported-modules))
                     decl))
                 (:declarations root-ast)))))