(ns cforge.check
  (:require [clojure.string :as str]
            [cforge.builtins :as builtins]
            [cforge.trace :as trace]))

(def integer-ranges
  {:i8  [(- 128N) 127N] :u8  [0N 255N]
   :i16 [(- 32768N) 32767N] :u16 [0N 65535N]
   :i32 [(- 2147483648N) 2147483647N] :u32 [0N 4294967295N]
   :i64 [(- 9223372036854775808N) 9223372036854775807N]
   :u64 [0N 18446744073709551615N]
   :isize [(- 9223372036854775808N) 9223372036854775807N]
   :usize [0N 18446744073709551615N]})

(def ^:dynamic *imports* #{})
(def ^:dynamic *functions* {})

(defn- diagnostic [category message span]
  {:category category :severity :error :message message :span span})

(defn- type-key [type-node]
  (when type-node
    (case (:node type-node)
      :named-type (let [parts (:name type-node)]
                    (if (= 1 (count parts))
                      (keyword (first parts))
                      (keyword (str/join "." parts))))
      :optional-type [:optional (type-key (:inner type-node))]
      :pointer-type [:pointer (type-key (:to type-node))]
      :reference-type [:reference (:mutable? type-node) (type-key (:to type-node))]
      nil)))

(defn- binding-type [binding]
  (if (map? binding) (:type binding) binding))

(defn- mutable-binding? [binding]
  (and (map? binding) (:mutable? binding)))

(defn- ensure-int-fits! [value t span]
  (if-let [[lo hi] (get integer-ranges t)]
    (when-not (<= lo value hi)
      (throw (ex-info "integer literal does not fit target type"
                      {:diagnostic (diagnostic :type/overflow
                                               (str value " does not fit " (name t))
                                               span)})))
    (throw (ex-info "unsupported integer type"
                    {:diagnostic (diagnostic :type/unsupported
                                             (str "unsupported integer type " t)
                                             span)}))))

(defn- require-type! [actual expected span]
  (when (not= actual expected)
    (throw (ex-info "type mismatch"
                    {:diagnostic (diagnostic :type/mismatch
                                             (str "expected " expected ", found " actual)
                                             span)}))))

(defn- require-integer-type! [t span]
  (when-not (contains? integer-ranges t)
    (throw (ex-info "integer type required"
                    {:diagnostic (diagnostic :type/mismatch
                                             (str "integer type required, found " t)
                                             span)}))))

(declare check-expr check-block check-statement)

(defn- check-negation [expr env expected]
  (let [t (or expected :i32)
        inner-expr (:expr expr)]
    (require-integer-type! t (:span expr))
    (if (= :integer-literal (:node inner-expr))
      (let [value (- (:value inner-expr))]
        (ensure-int-fits! value t (:span expr))
        (assoc expr
               :expr (assoc inner-expr :forge-type t :literal-negated? true)
               :forge-type t))
      (let [inner (check-expr inner-expr env t)]
        (assoc expr :expr inner :forge-type t)))))

(defn- check-args [expr env arg-types]
  (when-not (= (count arg-types) (count (:args expr)))
    (throw (ex-info "call arity mismatch"
                    {:diagnostic (diagnostic :type/call
                                             (str "expected " (count arg-types)
                                                  " arguments, found " (count (:args expr)))
                                             (:span expr))})))
  (mapv (fn [arg expected-type] (check-expr arg env expected-type))
        (:args expr) arg-types))

(defn- check-call [expr env expected]
  (if-let [signature (builtins/resolve-call expr *imports*)]
    (let [args (check-args expr env (:args signature))
          return-type (:return signature)]
      (when expected (require-type! return-type expected (:span expr)))
      (assoc expr :args args :forge-type return-type :builtin (:builtin signature)))
    (let [callee (:callee expr)
          function-name (when (= :name (:node callee)) (:name callee))
          signature (get *functions* function-name)]
      (when-not signature
        (throw (ex-info "call target unsupported"
                        {:diagnostic (diagnostic :type/unsupported
                                                 (str "unknown call target " (or function-name (:node callee)))
                                                 (:span expr))})))
      (let [args (check-args expr env (:params signature))
            return-type (:return signature)]
        (when expected (require-type! return-type expected (:span expr)))
        (assoc expr :args args :forge-type return-type :function function-name)))))

(defn check-expr [expr env expected]
  (case (:node expr)
    :integer-literal
    (let [t (or expected :i32)]
      (ensure-int-fits! (:value expr) t (:span expr))
      (assoc expr :forge-type t))

    :boolean-literal
    (do
      (when expected (require-type! :bool expected (:span expr)))
      (assoc expr :forge-type :bool))

    :string-literal
    (do
      (when expected (require-type! :str expected (:span expr)))
      (assoc expr :forge-type :str))

    :name
    (if-let [binding (get env (:name expr))]
      (let [t (binding-type binding)]
        (when expected (require-type! t expected (:span expr)))
        (assoc expr :forge-type t))
      (throw (ex-info "unresolved name"
                      {:diagnostic (diagnostic :name/unresolved
                                               (str "unresolved name " (:name expr))
                                               (:span expr))})))

    :call (check-call expr env expected)

    :unary
    (let [op (:op expr)]
      (cond
        (= op :not)
        (let [inner (check-expr (:expr expr) env :bool)]
          (assoc expr :expr inner :forge-type :bool))

        (= op :neg)
        (check-negation expr env expected)

        (contains? #{:pos :bit-not} op)
        (let [t (or expected :i32)]
          (require-integer-type! t (:span expr))
          (let [inner (check-expr (:expr expr) env t)]
            (assoc expr :expr inner :forge-type t)))

        :else
        (throw (ex-info "unsupported unary operator"
                        {:diagnostic (diagnostic :type/unsupported
                                                 (str "unsupported unary operator " op)
                                                 (:span expr))}))))

    :binary
    (let [op (:op expr)]
      (cond
        (contains? #{:add :sub :mul :div :rem :bit-and :bit-or :bit-xor :shl :shr} op)
        (let [t (or expected
                    (when (= :name (get-in expr [:left :node]))
                      (some-> (get env (get-in expr [:left :name])) binding-type))
                    :i32)]
          (require-integer-type! t (:span expr))
          (let [left (check-expr (:left expr) env t)
                right (check-expr (:right expr) env t)]
            (assoc expr :left left :right right :forge-type t)))

        (contains? #{:eq :neq} op)
        (let [left (check-expr (:left expr) env nil)
              t (:forge-type left)
              right (check-expr (:right expr) env t)]
          (when expected (require-type! :bool expected (:span expr)))
          (assoc expr :left left :right right :forge-type :bool :operand-type t))

        (contains? #{:lt :lte :gt :gte} op)
        (let [left (check-expr (:left expr) env nil)
              t (:forge-type left)]
          (require-integer-type! t (:span expr))
          (let [right (check-expr (:right expr) env t)]
            (when expected (require-type! :bool expected (:span expr)))
            (assoc expr :left left :right right :forge-type :bool :operand-type t)))

        (contains? #{:logical-and :logical-or} op)
        (let [left (check-expr (:left expr) env :bool)
              right (check-expr (:right expr) env :bool)]
          (when expected (require-type! :bool expected (:span expr)))
          (assoc expr :left left :right right :forge-type :bool))

        :else
        (throw (ex-info "unsupported binary operator"
                        {:diagnostic (diagnostic :type/unsupported
                                                 (str "unsupported binary operator " op)
                                                 (:span expr))}))))

    (throw (ex-info "expression not implemented in checker"
                    {:diagnostic (diagnostic :type/unsupported
                                             (str "checker does not yet support " (:node expr))
                                             (:span expr))}))))

(defn- check-assignment [stmt env]
  (let [target (:target stmt)]
    (when-not (= :name (:node target))
      (throw (ex-info "assignment target unsupported"
                      {:diagnostic (diagnostic :type/unsupported
                                               "bootstrap assignments require a local variable name"
                                               (:span target))})))
    (let [name (:name target)
          binding (get env name)]
      (when-not binding
        (throw (ex-info "unresolved assignment target"
                        {:diagnostic (diagnostic :name/unresolved
                                                 (str "unresolved name " name)
                                                 (:span target))})))
      (when-not (mutable-binding? binding)
        (throw (ex-info "assignment to immutable binding"
                        {:diagnostic (diagnostic :type/immutable
                                                 (str name " is not mutable")
                                                 (:span target))})))
      (let [t (binding-type binding)
            value (check-expr (:value stmt) env t)]
        [(assoc stmt
                :target (assoc target :forge-type t)
                :value value
                :forge-type t)
         env]))))

(defn check-statement [stmt env return-type]
  (case (:node stmt)
    :value-decl
    (let [decl-type (type-key (:type stmt))]
      (when-not decl-type
        (throw (ex-info "inferred declarations not implemented"
                        {:diagnostic (diagnostic :type/unsupported
                                                 "bootstrap checker requires an explicit value type"
                                                 (:span stmt))})))
      (let [init (check-expr (:init stmt) env decl-type)
            binding {:type decl-type :mutable? (= :var (:storage stmt))}]
        [(assoc stmt :declared-type decl-type :init init)
         (assoc env (:name stmt) binding)]))

    :assignment (check-assignment stmt env)

    :return
    (if-let [expr (:expr stmt)]
      (try
        [(assoc stmt :expr (check-expr expr env return-type)) env]
        (catch clojure.lang.ExceptionInfo e
          (if (= :type/mismatch (get-in (ex-data e) [:diagnostic :category]))
            (throw (ex-info "wrong return type"
                            {:diagnostic (diagnostic :type/return
                                                     "returned value does not match function return type"
                                                     (:span stmt))}))
            (throw e))))
      (do
        (when (and return-type (not= :void return-type))
          (throw (ex-info "missing return value"
                          {:diagnostic (diagnostic :type/return
                                                   "return value required"
                                                   (:span stmt))})))
        [stmt env]))

    :if
    (let [condition (check-expr (:condition stmt) env :bool)
          [then _] (check-block (:then stmt) env return-type)
          [else-branch _] (if-let [else-node (:else stmt)]
                            (if (= :block (:node else-node))
                              (check-block else-node env return-type)
                              (check-statement else-node env return-type))
                            [nil env])]
      [(assoc stmt :condition condition :then then :else else-branch) env])

    :while
    (let [condition (check-expr (:condition stmt) env :bool)
          [body _] (check-block (:body stmt) env return-type)]
      [(assoc stmt :condition condition :body body) env])

    :block
    (let [[block _] (check-block stmt env return-type)] [block env])

    :expression-statement
    [(assoc stmt :expr (check-expr (:expr stmt) env nil)) env]

    (throw (ex-info "statement not implemented in checker"
                    {:diagnostic (diagnostic :type/unsupported
                                             (str "checker does not yet support " (:node stmt))
                                             (:span stmt))}))))

(defn check-block [block env return-type]
  (loop [remaining (:statements block) env env checked []]
    (if-let [stmt (first remaining)]
      (let [[checked-stmt env'] (check-statement stmt env return-type)]
        (recur (next remaining) env' (conj checked checked-stmt)))
      [(assoc block :statements checked) env])))

(defn- always-returns-statement? [stmt]
  (case (:node stmt)
    :return true
    :block (boolean (some always-returns-statement? (:statements stmt)))
    :if (and (:else stmt)
             (always-returns-statement? (:then stmt))
             (always-returns-statement? (:else stmt)))
    false))

(defn- block-always-returns? [block]
  (boolean (some always-returns-statement? (:statements block))))

(defn- function-signature [f]
  {:params (mapv #(type-key (:type %)) (:params f))
   :return (type-key (:return-type f))})

(defn- check-function [f]
  (let [ret (type-key (:return-type f))
        param-env (reduce (fn [m p]
                            (assoc m (:name p)
                                   {:type (type-key (:type p)) :mutable? false}))
                          {} (:params f))
        [body _] (check-block (:body f) param-env ret)]
    (when (and ret (not= ret :void) (not (block-always-returns? body)))
      (throw (ex-info "not all paths return a value"
                      {:diagnostic (diagnostic :type/return
                                               "not all paths return a value"
                                               (:span f))})))
    (assoc f :resolved-return-type ret :body body)))

(defn- check-entry-point! [decls]
  (when-let [main (first (filter #(and (= :function-decl (:node %))
                                       (= "main" (:name %)))
                                 decls))]
    (when (seq (:params main))
      (throw (ex-info "main parameters unsupported in bootstrap"
                      {:diagnostic (diagnostic :type/main
                                               "bootstrap main must take no parameters"
                                               (:span main))})))
    (when (not= :i32 (:resolved-return-type main))
      (throw (ex-info "invalid main return type"
                      {:diagnostic (diagnostic :type/main
                                               "bootstrap main must return i32"
                                               (:span main))})))))

(defn check-file [ast]
  (trace/with-phase :check
    (try
      (let [imports (set (mapcat :names (:imports ast)))
            declarations (:declarations ast)
            names (map :name declarations)
            duplicate (first (for [[n xs] (group-by identity names)
                                   :when (> (count xs) 1)] n))]
        (when duplicate
          (throw (ex-info "duplicate top-level declaration"
                          {:diagnostic (diagnostic :name/duplicate
                                                   (str "duplicate declaration " duplicate)
                                                   (:span ast))})))
        (let [functions (into {}
                              (for [d declarations
                                    :when (= :function-decl (:node d))]
                                [(:name d) (function-signature d)]))]
          (binding [*imports* imports
                    *functions* functions]
            (let [decls (mapv (fn [d]
                                (case (:node d)
                                  :function-decl (check-function d)
                                  (throw (ex-info "top-level declaration unsupported"
                                                  {:diagnostic (diagnostic :type/unsupported
                                                                           (str "checker does not support " (:node d))
                                                                           (:span d))}))))
                              declarations)
                  _ (check-entry-point! decls)
                  checked (assoc ast :declarations decls)]
              (trace/emit! {:event :check/summary :declarations (count decls)})
              {:typed-ast checked :diagnostics []}))))
      (catch clojure.lang.ExceptionInfo e
        {:typed-ast nil
         :diagnostics [(or (:diagnostic (ex-data e))
                           (diagnostic :check/internal (.getMessage e) (:span ast)))]}))))
