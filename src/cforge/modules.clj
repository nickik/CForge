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

(defn- import-name [segments]
  (str/join "." segments))

(defn- module-short-name [module]
  (last (str/split module #"\.")))

(defn- builtin-module? [module]
  (or (= module "core")
      (= module "std")
      (str/starts-with? module "core.")
      (str/starts-with? module "std.")))

(defn- all-functions [ast]
  (into {}
        (for [decl (:declarations ast)
              :when (= :function-decl (:node decl))]
          [(:name decl) decl])))

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
               :functions (public-functions ast)
               :all-functions (all-functions ast)})))
   {}
   library-specs))

(declare rewrite-expr rewrite-statement qualify-local-expr qualify-local-statement)

(defn- member-path [expr]
  (case (:node expr)
    :name [(:name expr)]
    :member (when-let [target-path (member-path (:target expr))]
              (conj (vec target-path) (:member expr)))
    nil))

(defn- resolve-imported-module [reference imported-modules span]
  (cond
    (contains? imported-modules reference)
    reference

    :else
    (let [matches (vec (filter #(= reference (module-short-name %)) imported-modules))]
      (cond
        (= 1 (count matches)) (first matches)
        (> (count matches) 1)
        (throw (ex-info "ambiguous imported module alias"
                        {:diagnostic (diagnostic :module/ambiguous
                                                 (str "module alias " reference
                                                      " matches " (str/join ", " matches))
                                                 span)}))
        :else nil))))

(defn- imported-call [expr imported-modules]
  (let [callee (:callee expr)
        path (member-path callee)]
    (when (and path (>= (count path) 2))
      (let [function-name (last path)
            reference (str/join "." (butlast path))
            module (resolve-imported-module reference imported-modules (:span expr))]
        (when module
          [module function-name])))))

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

(defn qualify-local-expr [expr module local-functions]
  (if-not (map? expr)
    expr
    (case (:node expr)
      :call (let [callee (:callee expr)
                  callee' (if (and (= :name (:node callee))
                                   (contains? local-functions (:name callee)))
                            (assoc callee :name (str module "." (:name callee)))
                            (qualify-local-expr callee module local-functions))]
              (assoc expr
                     :callee callee'
                     :args (mapv #(qualify-local-expr % module local-functions) (:args expr))))
      :binary (assoc expr
                     :left (qualify-local-expr (:left expr) module local-functions)
                     :right (qualify-local-expr (:right expr) module local-functions))
      :unary (assoc expr :expr (qualify-local-expr (:expr expr) module local-functions))
      :member (assoc expr :target (qualify-local-expr (:target expr) module local-functions))
      :index (assoc expr
                    :target (qualify-local-expr (:target expr) module local-functions)
                    :index (qualify-local-expr (:index expr) module local-functions))
      expr)))

(defn- qualify-local-block [block module local-functions]
  (assoc block :statements
         (mapv #(qualify-local-statement % module local-functions) (:statements block))))

(defn qualify-local-statement [stmt module local-functions]
  (case (:node stmt)
    :value-decl (assoc stmt :init (qualify-local-expr (:init stmt) module local-functions))
    :assignment (assoc stmt
                       :target (qualify-local-expr (:target stmt) module local-functions)
                       :value (qualify-local-expr (:value stmt) module local-functions))
    :return (cond-> stmt
              (:expr stmt) (assoc :expr (qualify-local-expr (:expr stmt) module local-functions)))
    :expression-statement (assoc stmt :expr (qualify-local-expr (:expr stmt) module local-functions))
    :while (assoc stmt
                  :condition (qualify-local-expr (:condition stmt) module local-functions)
                  :body (qualify-local-block (:body stmt) module local-functions))
    :if (assoc stmt
               :condition (qualify-local-expr (:condition stmt) module local-functions)
               :then (qualify-local-block (:then stmt) module local-functions)
               :else (when-let [else (:else stmt)]
                       (if (= :block (:node else))
                         (qualify-local-block else module local-functions)
                         (qualify-local-statement else module local-functions))))
    :block (qualify-local-block stmt module local-functions)
    stmt))

(defn- local-library-imports [library libraries]
  (set
   (for [segments (mapcat :names (get-in library [:ast :imports]))
         :let [module (import-name segments)]
         :when (contains? libraries module)]
     module)))

(defn- linked-module-closure [initial libraries]
  (letfn [(visit [module seen]
            (if (contains? seen module)
              seen
              (let [seen' (conj seen module)
                    deps (if-let [library (get libraries module)]
                           (local-library-imports library libraries)
                           #{})]
                (reduce (fn [state dependency]
                          (visit dependency state))
                        seen'
                        deps))))]
    (reduce (fn [seen module]
              (if (contains? libraries module)
                (visit module seen)
                seen))
            #{}
            initial)))

(defn- qualified-library-declarations [module library libraries]
  (let [functions (:all-functions library)
        local-names (set (keys functions))
        imported-modules (local-library-imports library libraries)]
    (mapv (fn [[name function]]
            (let [rewritten-body (rewrite-block (:body function) libraries imported-modules)]
              (-> function
                  (assoc :name (str module "." name)
                         :linked-module module
                         :body (qualify-local-block rewritten-body module local-names)))))
          functions)))

(defn link-root
  "Resolve root imports against supplied libraries and link the full transitive
   local-library closure into the root compilation unit. Only public functions
   may be referenced across module boundaries; private helpers are retained
   under qualified internal names for same-library calls. Forge imports expose
   both their full path and, when unambiguous, the final module component as the
   source-level namespace. Builtin core.* and std.* imports stay on the
   interpreter's existing builtin path."
  [root-ast libraries]
  (let [root-imports (:imports root-ast)
        all-imported-modules (set (map import-name (mapcat :names root-imports)))
        linked-root-modules (set (filter #(contains? libraries %) all-imported-modules))]
    (doseq [module all-imported-modules]
      (when-not (or (contains? libraries module)
                    (builtin-module? module))
        (throw (ex-info "unresolved imported module"
                        {:diagnostic (diagnostic :module/missing
                                                 (str "no library supplied for import " module)
                                                 (:span root-ast))}))))
    (let [linked-modules (sort (linked-module-closure linked-root-modules libraries))
          library-imports (mapcat #(get-in libraries [% :ast :imports]) linked-modules)
          linked-decls (mapcat #(qualified-library-declarations % (get libraries %) libraries)
                               linked-modules)
          rewritten-root (mapv (fn [decl]
                                 (if (= :function-decl (:node decl))
                                   (assoc decl :body (rewrite-block (:body decl) libraries linked-root-modules))
                                   decl))
                               (:declarations root-ast))]
      (assoc root-ast
             :imports (vec (concat root-imports library-imports))
             :declarations (vec (concat rewritten-root linked-decls))))))