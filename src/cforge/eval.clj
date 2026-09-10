(ns cforge.eval
  (:require [cforge.trace :as trace]))

(defn- forge-value [type value] {:forge-type type :value value})
(defn- truth [v] (boolean (:value v)))

(defn- range-for [t]
  (case t
    :i8 [(- 128N) 127N] :u8 [0N 255N]
    :i16 [(- 32768N) 32767N] :u16 [0N 65535N]
    :i32 [(- 2147483648N) 2147483647N] :u32 [0N 4294967295N]
    :i64 [(- 9223372036854775808N) 9223372036854775807N]
    :u64 [0N 18446744073709551615N]
    nil))

(defn- checked-int [t n span]
  (if-let [[lo hi] (range-for t)]
    (if (<= lo n hi)
      (forge-value t n)
      (throw (ex-info "integer overflow"
                      {:diagnostic {:category :runtime/overflow :severity :error
                                    :message (str "integer overflow for " t)
                                    :span span}})))
    (throw (ex-info "not an integer type"
                    {:diagnostic {:category :runtime/type :severity :error
                                  :message (str "not an integer type: " t)
                                  :span span}}))))

(declare eval-expr eval-block)

(defn- eval-binary [expr env]
  (let [op (:op expr)]
    (case op
      :logical-and
      (let [l (eval-expr (:left expr) env)]
        (if (truth l)
          (forge-value :bool (truth (eval-expr (:right expr) env)))
          (forge-value :bool false)))

      :logical-or
      (let [l (eval-expr (:left expr) env)]
        (if (truth l)
          (forge-value :bool true)
          (forge-value :bool (truth (eval-expr (:right expr) env)))))

      (let [l (eval-expr (:left expr) env)
            r (eval-expr (:right expr) env)
            a (:value l) b (:value r)
            t (:forge-type l)]
        (case op
          :add (checked-int t (+ a b) (:span expr))
          :sub (checked-int t (- a b) (:span expr))
          :mul (checked-int t (* a b) (:span expr))
          :div (if (zero? b)
                 (throw (ex-info "division by zero"
                                 {:diagnostic {:category :runtime/divide-by-zero
                                               :severity :error
                                               :message "division by zero"
                                               :span (:span expr)}}))
                 (checked-int t (quot a b) (:span expr)))
          :rem (if (zero? b)
                 (throw (ex-info "division by zero"
                                 {:diagnostic {:category :runtime/divide-by-zero
                                               :severity :error
                                               :message "remainder by zero"
                                               :span (:span expr)}}))
                 (checked-int t (rem a b) (:span expr)))
          :eq (forge-value :bool (= a b))
          :neq (forge-value :bool (not= a b))
          :lt (forge-value :bool (< a b))
          :lte (forge-value :bool (<= a b))
          :gt (forge-value :bool (> a b))
          :gte (forge-value :bool (>= a b))
          :bit-and (checked-int t (bit-and a b) (:span expr))
          :bit-or (checked-int t (bit-or a b) (:span expr))
          :bit-xor (checked-int t (bit-xor a b) (:span expr))
          :shl (checked-int t (bit-shift-left a (int b)) (:span expr))
          :shr (checked-int t (bit-shift-right a (int b)) (:span expr))
          (throw (ex-info "operator not implemented"
                          {:diagnostic {:category :runtime/unsupported
                                        :severity :error
                                        :message (str "operator not implemented: " op)
                                        :span (:span expr)}})))))))

(defn eval-expr [expr env]
  (trace/emit! {:event :eval/expression :kind (:node expr) :span (:span expr)})
  (case (:node expr)
    :integer-literal (forge-value (:forge-type expr) (:value expr))
    :boolean-literal (forge-value :bool (:value expr))
    :name (or (get env (:name expr))
              (throw (ex-info "unresolved runtime name"
                              {:diagnostic {:category :runtime/unresolved
                                            :severity :error
                                            :message (str "unresolved name " (:name expr))
                                            :span (:span expr)}})))
    :binary (eval-binary expr env)
    :unary (let [v (eval-expr (:expr expr) env)]
             (case (:op expr)
               :not (forge-value :bool (not (truth v)))
               :neg (checked-int (:forge-type expr) (- (:value v)) (:span expr))
               :pos v
               :bit-not (checked-int (:forge-type expr) (bit-not (:value v)) (:span expr))
               (throw (ex-info "unary operator not executable yet"
                               {:diagnostic {:category :runtime/unsupported
                                             :severity :error
                                             :message (str "unsupported unary operator " (:op expr))
                                             :span (:span expr)}}))))
    (throw (ex-info "expression not executable yet"
                    {:diagnostic {:category :runtime/unsupported :severity :error
                                  :message (str "cannot evaluate " (:node expr))
                                  :span (:span expr)}}))))

(defn- eval-statement [stmt env]
  (case (:node stmt)
    :value-decl
    (let [value (eval-expr (:init stmt) env)]
      [{:flow :normal} (assoc env (:name stmt) value)])

    :return
    [{:flow :return :value (when (:expr stmt) (eval-expr (:expr stmt) env))} env]

    :if
    (let [condition (eval-expr (:condition stmt) env)]
      (if (truth condition)
        (let [[flow _] (eval-block (:then stmt) env)] [flow env])
        (if-let [else-node (:else stmt)]
          (if (= :block (:node else-node))
            (let [[flow _] (eval-block else-node env)] [flow env])
            (eval-statement else-node env))
          [{:flow :normal} env])))

    :block
    (let [[flow _] (eval-block stmt env)] [flow env])

    :expression-statement
    (do (eval-expr (:expr stmt) env) [{:flow :normal} env])

    (throw (ex-info "statement not executable yet"
                    {:diagnostic {:category :runtime/unsupported :severity :error
                                  :message (str "cannot execute " (:node stmt))
                                  :span (:span stmt)}}))))

(defn eval-block [block env]
  (loop [stmts (:statements block) env env]
    (if-let [stmt (first stmts)]
      (let [[flow env'] (eval-statement stmt env)]
        (if (= :normal (:flow flow))
          (recur (next stmts) env')
          [flow env']))
      [{:flow :normal} env])))

(defn eval-main [typed-ast]
  (trace/with-phase :eval
    (try
      (let [main (first (filter #(and (= :function-decl (:node %))
                                      (= "main" (:name %)))
                                (:declarations typed-ast)))]
        (when-not main
          (throw (ex-info "main not found"
                          {:diagnostic {:category :name/main-missing :severity :error
                                        :message "main function not found"
                                        :span (:span typed-ast)}})))
        (when (seq (:params main))
          (throw (ex-info "bootstrap main arguments unsupported"
                          {:diagnostic {:category :runtime/unsupported :severity :error
                                        :message "bootstrap evaluator supports only main()"
                                        :span (:span main)}})))
        (let [[flow _] (eval-block (:body main) {})]
          (if (= :return (:flow flow))
            (let [value (:value flow)]
              (trace/emit! {:event :eval/return :value value})
              {:value value :exit (int (:value value)) :diagnostics []})
            {:value nil :exit 0 :diagnostics []})))
      (catch clojure.lang.ExceptionInfo e
        {:value nil :exit nil
         :diagnostics [(or (:diagnostic (ex-data e))
                           {:category :runtime/internal :severity :error
                            :message (.getMessage e)})]}))))
