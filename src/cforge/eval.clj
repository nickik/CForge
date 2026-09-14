(ns cforge.eval
  (:require [cforge.builtins :as builtins]
            [cforge.trace :as trace]))

(def ^:dynamic *functions* {})

(defn- forge-value [type value] {:forge-type type :value value})
(defn- truth [v] (boolean (:value v)))

(defn- range-for [t]
  (case t
    :i8 [(- 128N) 127N] :u8 [0N 255N]
    :i16 [(- 32768N) 32767N] :u16 [0N 65535N]
    :i32 [(- 2147483648N) 2147483647N] :u32 [0N 4294967295N]
    :i64 [(- 9223372036854775808N) 9223372036854775807N]
    :u64 [0N 18446744073709551615N]
    :isize [(- 9223372036854775808N) 9223372036854775807N]
    :usize [0N 18446744073709551615N]
    nil))

(defn- width-for [t]
  (cond
    (contains? #{:i8 :u8} t) 8
    (contains? #{:i16 :u16} t) 16
    (contains? #{:i32 :u32} t) 32
    (contains? #{:i64 :u64 :isize :usize} t) 64
    :else nil))

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

(defn- divide-by-zero! [span what]
  (throw (ex-info "division by zero"
                  {:diagnostic {:category :runtime/divide-by-zero
                                :severity :error
                                :message what
                                :span span}})))

(defn- as-big-integer ^java.math.BigInteger [n]
  (cond
    (instance? java.math.BigInteger n) n
    (instance? clojure.lang.BigInt n) (.toBigInteger ^clojure.lang.BigInt n)
    :else (java.math.BigInteger/valueOf (long n))))

(defn- big-and [a b]
  (.and (as-big-integer a) (as-big-integer b)))
(defn- big-or [a b]
  (.or (as-big-integer a) (as-big-integer b)))
(defn- big-xor [a b]
  (.xor (as-big-integer a) (as-big-integer b)))
(defn- big-not [a]
  (.not (as-big-integer a)))
(defn- big-shift-left [a n]
  (.shiftLeft (as-big-integer a) n))
(defn- big-shift-right [a n]
  (.shiftRight (as-big-integer a) n))

(defn- checked-shift-count! [t n span]
  (let [width (width-for t)]
    (when (or (nil? width) (neg? n) (>= n width))
      (throw (ex-info "shift count out of range"
                      {:diagnostic {:category :runtime/shift-out-of-range :severity :error
                                    :message (str "shift count " n " is invalid for " t)
                                    :span span}})))))

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
                 (divide-by-zero! (:span expr) "division by zero")
                 (checked-int t (quot a b) (:span expr)))
          :rem (if (zero? b)
                 (divide-by-zero! (:span expr) "remainder by zero")
                 (checked-int t (rem a b) (:span expr)))
          :eq (forge-value :bool (= a b))
          :neq (forge-value :bool (not= a b))
          :lt (forge-value :bool (< a b))
          :lte (forge-value :bool (<= a b))
          :gt (forge-value :bool (> a b))
          :gte (forge-value :bool (>= a b))
          :bit-and (checked-int t (big-and a b) (:span expr))
          :bit-or (checked-int t (big-or a b) (:span expr))
          :bit-xor (checked-int t (big-xor a b) (:span expr))
          :shl (do (checked-shift-count! t b (:span expr))
                   (checked-int t (big-shift-left a (int b)) (:span expr)))
          :shr (do (checked-shift-count! t b (:span expr))
                   (checked-int t (big-shift-right a (int b)) (:span expr)))
          (throw (ex-info "operator not implemented"
                          {:diagnostic {:category :runtime/unsupported
                                        :severity :error
                                        :message (str "operator not implemented: " op)
                                        :span (:span expr)}})))))))

(defn- eval-function-call [expr env]
  (let [name (:function expr)
        function (get *functions* name)]
    (when-not function
      (throw (ex-info "function not found at runtime"
                      {:diagnostic {:category :runtime/unresolved
                                    :severity :error
                                    :message (str "function not found: " name)
                                    :span (:span expr)}})))
    (let [values (mapv #(eval-expr % env) (:args expr))
          call-env (zipmap (map :name (:params function)) values)
          [flow _] (eval-block (:body function) call-env)]
      (cond
        (= :return (:flow flow))
        (or (:value flow) (forge-value :void nil))

        (= :void (:resolved-return-type function))
        (forge-value :void nil)

        :else
        (throw (ex-info "function completed without return"
                        {:diagnostic {:category :runtime/missing-return
                                      :severity :error
                                      :message (str name " completed without returning a value")
                                      :span (:span function)}}))))))

(defn- eval-call [expr env]
  (if-let [builtin (:builtin expr)]
    (let [values (mapv #(eval-expr % env) (:args expr))
          raw (builtins/invoke builtin (mapv :value values))]
      (forge-value (:forge-type expr) raw))
    (eval-function-call expr env)))

(defn eval-expr [expr env]
  (trace/emit! {:event :eval/expression :kind (:node expr) :span (:span expr)})
  (case (:node expr)
    :integer-literal (forge-value (:forge-type expr) (:value expr))
    :boolean-literal (forge-value :bool (:value expr))
    :string-literal (forge-value :str (:value expr))
    :name (or (get env (:name expr))
              (throw (ex-info "unresolved runtime name"
                              {:diagnostic {:category :runtime/unresolved
                                            :severity :error
                                            :message (str "unresolved name " (:name expr))
                                            :span (:span expr)}})))
    :call (eval-call expr env)
    :binary (eval-binary expr env)
    :unary (let [v (eval-expr (:expr expr) env)]
             (case (:op expr)
               :not (forge-value :bool (not (truth v)))
               :neg (checked-int (:forge-type expr) (- (:value v)) (:span expr))
               :pos v
               :bit-not (checked-int (:forge-type expr) (big-not (:value v)) (:span expr))
               (throw (ex-info "unary operator not executable yet"
                               {:diagnostic {:category :runtime/unsupported
                                             :severity :error
                                             :message (str "unsupported unary operator " (:op expr))
                                             :span (:span expr)}}))))
    (throw (ex-info "expression not executable yet"
                    {:diagnostic {:category :runtime/unsupported :severity :error
                                  :message (str "cannot evaluate " (:node expr))
                                  :span (:span expr)}}))))

(defn- eval-assignment [stmt env]
  (let [name (get-in stmt [:target :name])
        value (eval-expr (:value stmt) env)]
    [{:flow :normal} (assoc env name value)]))

(defn- eval-statement [stmt env]
  (case (:node stmt)
    :value-decl
    (let [value (eval-expr (:init stmt) env)]
      [{:flow :normal} (assoc env (:name stmt) value)])

    :assignment
    (eval-assignment stmt env)

    :return
    [{:flow :return :value (when (:expr stmt) (eval-expr (:expr stmt) env))} env]

    :if
    (let [condition (eval-expr (:condition stmt) env)]
      (if (truth condition)
        (eval-block (:then stmt) env)
        (if-let [else-node (:else stmt)]
          (if (= :block (:node else-node))
            (eval-block else-node env)
            (eval-statement else-node env))
          [{:flow :normal} env])))

    :while
    (loop [env env iterations 0]
      (when (>= iterations 1000000)
        (throw (ex-info "loop iteration limit exceeded"
                        {:diagnostic {:category :runtime/loop-limit
                                      :severity :error
                                      :message "bootstrap interpreter loop exceeded 1000000 iterations"
                                      :span (:span stmt)}})))
      (if (truth (eval-expr (:condition stmt) env))
        (let [[flow env'] (eval-block (:body stmt) env)]
          (if (= :normal (:flow flow))
            (recur env' (inc iterations))
            [flow env']))
        [{:flow :normal} env]))

    :block
    (eval-block stmt env)

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
      (let [functions (into {}
                            (for [d (:declarations typed-ast)
                                  :when (= :function-decl (:node d))]
                              [(:name d) d]))
            main (get functions "main")]
        (when-not main
          (throw (ex-info "main not found"
                          {:diagnostic {:category :name/main-missing :severity :error
                                        :message "main function not found"
                                        :span (:span typed-ast)}})))
        (binding [*functions* functions]
          (let [[flow _] (eval-block (:body main) {})]
            (if (= :return (:flow flow))
              (let [value (:value flow)]
                (trace/emit! {:event :eval/return :value value})
                {:value value :exit (int (:value value)) :diagnostics []})
              (throw (ex-info "main completed without return"
                              {:diagnostic {:category :runtime/missing-return :severity :error
                                            :message "main completed without returning i32"
                                            :span (:span main)}}))))))
      (catch clojure.lang.ExceptionInfo e
        {:value nil :exit nil
         :diagnostics [(or (:diagnostic (ex-data e))
                           {:category :runtime/internal :severity :error
                            :message (.getMessage e)})]}))))
