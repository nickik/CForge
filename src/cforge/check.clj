(ns cforge.check
  (:require [cforge.trace :as trace]))

(def integer-types
  {:i8  {:bits 8 :signed? true}
   :u8  {:bits 8 :signed? false}
   :i16 {:bits 16 :signed? true}
   :u16 {:bits 16 :signed? false}
   :i32 {:bits 32 :signed? true}
   :u32 {:bits 32 :signed? false}
   :i64 {:bits 64 :signed? true}
   :u64 {:bits 64 :signed? false}})

(defn- diagnostic [category message span]
  {:category category :severity :error :message message :span span})

(defn- type-key [type-node]
  (when type-node
    (case (:node type-node)
      :named-type (let [parts (:name type-node)]
                    (if (= 1 (count parts))
                      (keyword (first parts))
                      (keyword (clojure.string/join "." parts))))
      :optional-type [:optional (type-key (:inner type-node))]
      :pointer-type [:pointer (type-key (:to type-node))]
      :reference-type [:reference (:mutable? type-node) (type-key (:to type-node))]
      nil)))

(defn- int-range [t]
  (let [{:keys [bits signed?]} (get integer-types t)]
    (when bits
      (if signed?
        [(- (bit-shift-left 1N (dec bits)))
         (dec (bit-shift-left 1N (dec bits)))]
        [0N (dec (bit-shift-left 1N bits))]))))

(defn- ensure-int-fits! [value t span]
  (if-let [[lo hi] (int-range t)]
    (when (or (< value lo) (> value hi))
      (throw (ex-info "integer literal does not fit target type"
                      {:diagnostic (diagnostic :type/overflow
                                               (str value " does not fit " (name t))
                                               span)})))
    (throw (ex-info "unsupported integer type"
                    {:diagnostic (diagnostic :type/unsupported
                                             (str "unsupported integer type " t)
                                             span)}))))

(declare check-expr check-block)

(defn- require-type! [actual expected span]
  (when (not= actual expected)
    (throw (ex-info "type mismatch"
                    {:diagnostic (diagnostic :type/mismatch
                                             (str "expected " expected ", found " actual)
                                             span)}))))

(defn- check-expr [expr env expected]
  (case (:node expr)
    :integer-literal
    (let [t (or expected :i32)]
      (ensure-int-fits! (:value expr) t (:span expr))
      (assoc expr :forge-type t))

    :boolean-literal
    (do (when expected (require-type! :bool expected (:span expr)))
        (assoc expr :forge-type :bool))

    :name
    (if-let [t (get env (:name expr))]
      (do (when expected (require-type! t expected (:span expr)))
          (assoc expr :forge-type t))
      (throw (ex-info "unresolved name"
                      {:diagnostic (diagnostic :name/unresolved
                                               (str "unresolved name " (:name expr))
                                               (:span expr))})))

    :unary
    (let [op (:op expr)]
      (case op
        :not (let [inner (check-expr (:expr expr) env :bool)]
               (assoc expr :expr inner :forge-type :bool))
        (:neg :pos :bit-not)
        (let [t (or expected :i32)
              inner (check-expr (:expr expr) env t)]
          (assoc expr :expr inner :forge-type t))
        (throw (ex-info "unsupported unary operator"
                        {:diagnostic (diagnostic :type/unsupported
                                                 (str "unsupported unary operator " op)
                                                 (:span expr))}))))

    :binary
    (let [op (:op expr)]
      (cond
        (contains? #{:add :sub :mul :div :rem :bit-and :bit-or :bit-xor :shl :shr} op)
        (let [t (or expected :i32)
              left (check-expr (:left expr) env t)
              right (check-expr (:right expr) env t)]
          (assoc expr :left left :right right :forge-type t))

        (contains? #{:eq :neq :lt :lte :gt :gte} op)
        (let [left (check-expr (:left expr) env nil)
              t (:forge-type left)
              right (check-expr (:right expr) env t)]
          (when expected (require-type! :bool expected (:span expr)))
          (assoc expr :left left :right right :forge-type :bool :operand-type t))

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

(defn- check-statement [stmt env return-type]
  (case (:node stmt)
    :value-decl
    (let [decl-type (type-key (:type stmt))]
      (when-not decl-type
        (throw (ex-info "inferred declarations not implemented"
                        {:diagnostic (diagnostic :type/unsupported
                                                 "bootstrap checker requires an explicit value type"
                                                 (:span stmt))})))
      (let [init (check-expr (:init stmt) env decl-type)]
        [(assoc stmt :declared-type decl-type :init init)
         (assoc env (:name stmt) decl-type)]))

    :return
    (let [expr (:expr stmt)]
      (if expr
        [(assoc stmt :expr (check-expr expr env return-type)) env]
        (do
          (when return-type
            (throw (ex-info "missing return value"
                            {:diagnostic (diagnostic :type/return
                                                     "return value required"
                                                     (:span stmt))})))
          [stmt env])))

    :if
    (let [condition (check-expr (:condition stmt) env :bool)
          [then _] (check-block (:then stmt) env return-type)
          [else-branch _] (if (:else stmt)
                            (if (= :block (get-in stmt [:else :node]))
                              (check-block (:else stmt) env return-type)
                              (check-statement (:else stmt) env return-type))
                            [nil env])]
      [(assoc stmt :condition condition :then then :else else-branch) env])

    :block
    (let [[block _] (check-block stmt env return-type)] [block env])

    :expression-statement
    [(assoc stmt :expr (check-expr (:expr stmt) env nil)) env]

    (throw (ex-info "statement not implemented in checker"
                    {:diagnostic (diagnostic :type/unsupported
                                             (str "checker does not yet support " (:node stmt))
                                             (:span stmt))}))))

(defn- check-block [block env return-type]
  (loop [remaining (:statements block) env env checked []]
    (if-let [stmt (first remaining)]
      (let [[stmt env] (check-statement stmt env return-type)]
        (recur (next remaining) env (conj checked stmt)))
      [(assoc block :statements checked) env])))

(defn- check-function [f]
  (let [ret (type-key (:return-type f))
        param-env (reduce (fn [m p] (assoc m (:name p) (type-key (:type p)))) {} (:params f))
        [body _] (check-block (:body f) param-env ret)]
    (assoc f :resolved-return-type ret :body body)))

(defn check-file [ast]
  (trace/with-phase :check
    (try
      (let [names (map :name (:declarations ast))
            duplicates (seq (for [[n xs] (group-by identity names) :when (> (count xs) 1)] n))]
        (when duplicates
          (throw (ex-info "duplicate top-level declaration"
                          {:diagnostic (diagnostic :name/duplicate
                                                   (str "duplicate declaration " (first duplicates))
                                                   (:span ast))})))
        (let [decls (mapv (fn [d]
                            (case (:node d)
                              :function-decl (check-function d)
                              (throw (ex-info "top-level declaration unsupported"
                                              {:diagnostic (diagnostic :type/unsupported
                                                                       (str "checker does not support " (:node d))
                                                                       (:span d))}))))
                          (:declarations ast))
              checked (assoc ast :declarations decls)]
          (trace/emit! {:event :check/summary :declarations (count decls)})
          {:typed-ast checked :diagnostics []}))
      (catch clojure.lang.ExceptionInfo e
        {:typed-ast nil
         :diagnostics [(or (:diagnostic (ex-data e))
                           (diagnostic :check/internal (.getMessage e) (:span ast)))]}))))
