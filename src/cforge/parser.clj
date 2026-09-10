(ns cforge.parser
  (:require [cforge.trace :as trace]))

(defn- current [p] (nth (:tokens p) (:pos p)))
(defn- kind [p] (:kind (current p)))
(defn- advance [p] (update p :pos inc))
(defn- at? [p k] (= (kind p) k))

(defn- fail! [p category message]
  (let [t (current p)
        d {:category category :severity :error :message message :span (:span t)}]
    (throw (ex-info message {:diagnostic d}))))

(defn- expect [p k]
  (if (at? p k)
    [(current p) (advance p)]
    (fail! p :parse/unexpected-token
           (str "expected " (name k) ", found " (name (kind p))))))

(defn- match-token [p k]
  (when (at? p k) [(current p) (advance p)]))

(defn- span-between [start-token end-token]
  {:start (get-in start-token [:span :start])
   :end (get-in end-token [:span :end])
   :line (get-in start-token [:span :line])
   :column (get-in start-token [:span :column])})

(defn- span-nodes [a b]
  {:start (get-in a [:span :start])
   :end (get-in b [:span :end])
   :line (get-in a [:span :line])
   :column (get-in a [:span :column])})

(declare parse-expression parse-statement parse-block)

(def infix-binding
  {:or-or [10 11 :logical-or]
   :and-and [20 21 :logical-and]
   :pipe [30 31 :bit-or]
   :caret [40 41 :bit-xor]
   :amp [50 51 :bit-and]
   :eq-eq [60 61 :eq]
   :not-eq [60 61 :neq]
   :lt [70 71 :lt]
   :lte [70 71 :lte]
   :gt [70 71 :gt]
   :gte [70 71 :gte]
   :shl [80 81 :shl]
   :shr [80 81 :shr]
   :plus [90 91 :add]
   :minus [90 91 :sub]
   :star [100 101 :mul]
   :slash [100 101 :div]
   :percent [100 101 :rem]})

(def prefix-ops
  {:bang :not :minus :neg :plus :pos :tilde :bit-not :star :deref :amp :address-of})

(defn- parse-qualified-name [p]
  (let [[first-t p] (expect p :identifier)]
    (loop [segments [(:text first-t)] p p last-t first-t]
      (if (at? p :dot)
        (let [[_ p] (expect p :dot)
              [id p] (expect p :identifier)]
          (recur (conj segments (:text id)) p id))
        [{:node :qualified-name :segments segments :span (span-between first-t last-t)} p]))))

(defn- parse-type [p]
  (let [start (current p)]
    (cond
      (at? p :star)
      (let [[_ p] (expect p :star)
            [inner p] (parse-type p)
            node {:node :pointer-type :to inner :span (span-between start {:span (:span inner)})}]
        [node p])

      (at? p :amp)
      (let [[_ p] (expect p :amp)
            mutable? (at? p :mut)
            p (if mutable? (advance p) p)
            [inner p] (parse-type p)
            node {:node :reference-type :mutable? mutable? :to inner
                  :span {:start (get-in start [:span :start])
                         :end (get-in inner [:span :end])
                         :line (get-in start [:span :line])
                         :column (get-in start [:span :column])}}]
        [node p])

      :else
      (let [[name p] (parse-qualified-name p)
            optional? (at? p :question)
            p (if optional? (advance p) p)
            base {:node :named-type :name (:segments name) :span (:span name)}]
        [(if optional?
           {:node :optional-type :inner base
            :span (assoc (:span base) :end (get-in (nth (:tokens p) (dec (:pos p))) [:span :end]))}
           base)
         p]))))

(defn- parse-primary [p]
  (let [t (current p)]
    (case (:kind t)
      :int [{:node :integer-literal :value (:value t) :text (:text t) :span (:span t)} (advance p)]
      :float [{:node :float-literal :value (:value t) :text (:text t) :span (:span t)} (advance p)]
      :string [{:node :string-literal :value (:value t) :span (:span t)} (advance p)]
      :true [{:node :boolean-literal :value true :span (:span t)} (advance p)]
      :false [{:node :boolean-literal :value false :span (:span t)} (advance p)]
      :identifier [{:node :name :name (:text t) :span (:span t)} (advance p)]
      :lparen (let [[_ p] (expect p :lparen)
                    [expr p] (parse-expression p 0)
                    [close p] (expect p :rparen)]
                [(assoc expr :span (span-between t close)) p])
      (if-let [op (get prefix-ops (:kind t))]
        (let [p (advance p)
              [expr p] (parse-expression p 110)]
          [{:node :unary :op op :expr expr
            :span {:start (get-in t [:span :start])
                   :end (get-in expr [:span :end])
                   :line (get-in t [:span :line])
                   :column (get-in t [:span :column])}}
           p])
        (fail! p :parse/unexpected-token
               (str "expected expression, found " (name (:kind t))))))))

(defn- parse-arguments [p]
  (let [[_ p] (expect p :lparen)]
    (if (at? p :rparen)
      (let [[close p] (expect p :rparen)] [[] close p])
      (loop [args [] p p]
        (let [[arg p] (parse-expression p 0)
              args (conj args arg)]
          (if (at? p :comma)
            (recur args (advance p))
            (let [[close p] (expect p :rparen)] [args close p])))))))

(defn parse-expression
  ([p] (parse-expression p 0))
  ([p min-bp]
   (let [[lhs p] (parse-primary p)]
     (loop [lhs lhs p p]
       (cond
         (at? p :lparen)
         (let [[args close p] (parse-arguments p)
               node {:node :call :callee lhs :args args
                     :span {:start (get-in lhs [:span :start])
                            :end (get-in close [:span :end])
                            :line (get-in lhs [:span :line])
                            :column (get-in lhs [:span :column])}}]
           (recur node p))

         (at? p :lbracket)
         (let [[_ p] (expect p :lbracket)
               [idx p] (parse-expression p 0)
               [close p] (expect p :rbracket)
               node {:node :index :target lhs :index idx
                     :span {:start (get-in lhs [:span :start])
                            :end (get-in close [:span :end])
                            :line (get-in lhs [:span :line])
                            :column (get-in lhs [:span :column])}}]
           (recur node p))

         (at? p :dot)
         (let [[_ p] (expect p :dot)
               [member p] (expect p :identifier)
               node {:node :member :target lhs :member (:text member)
                     :span {:start (get-in lhs [:span :start])
                            :end (get-in member [:span :end])
                            :line (get-in lhs [:span :line])
                            :column (get-in lhs [:span :column])}}]
           (recur node p))

         :else
         (if-let [[lbp rbp op] (get infix-binding (kind p))]
           (if (< lbp min-bp)
             [lhs p]
             (let [op-token (current p)
                   p (advance p)
                   [rhs p] (parse-expression p rbp)
                   node {:node :binary :op op :left lhs :right rhs
                         :span (span-nodes lhs rhs)}]
               (trace/emit! {:event :parse/expression :kind :binary :op op :span (:span node)})
               (recur node p)))
           [lhs p]))))))

(defn- parse-value-decl [p]
  (let [start (current p)
        storage (:kind start)
        p (advance p)
        [name-t p] (expect p :identifier)
        [type p] (if (at? p :colon)
                   (parse-type (advance p))
                   [nil p])
        [_ p] (expect p :eq)
        [init p] (parse-expression p 0)
        [semi p] (expect p :semicolon)
        node {:node :value-decl :storage storage :name (:text name-t)
              :type type :init init :span (span-between start semi)}]
    (trace/emit! {:event :parse/node :kind :value-decl :name (:name node) :span (:span node)})
    [node p]))

(defn- parse-return [p]
  (let [start (current p)
        p (advance p)
        tail? (at? p :tail)
        p (if tail? (advance p) p)
        [expr p] (if (at? p :semicolon) [nil p] (parse-expression p 0))
        [semi p] (expect p :semicolon)]
    [{:node :return :tail? tail? :expr expr :span (span-between start semi)} p]))

(defn- parse-if [p]
  (let [start (current p)
        [_ p] (expect p :if)
        [_ p] (expect p :lparen)
        [condition p] (parse-expression p 0)
        [_ p] (expect p :rparen)
        [then p] (parse-block p)
        [else-branch p] (if (at? p :else)
                          (let [p (advance p)]
                            (if (at? p :if)
                              (parse-if p)
                              (parse-block p)))
                          [nil p])
        end-node (or else-branch then)]
    [{:node :if :condition condition :then then :else else-branch
      :span {:start (get-in start [:span :start])
             :end (get-in end-node [:span :end])
             :line (get-in start [:span :line])
             :column (get-in start [:span :column])}}
     p]))

(defn parse-statement [p]
  (case (kind p)
    (:val :var :const) (parse-value-decl p)
    :return (parse-return p)
    :if (parse-if p)
    :lbrace (parse-block p)
    (let [[expr p] (parse-expression p 0)
          [semi p] (expect p :semicolon)]
      [{:node :expression-statement :expr expr
        :span {:start (get-in expr [:span :start])
               :end (get-in semi [:span :end])
               :line (get-in expr [:span :line])
               :column (get-in expr [:span :column])}}
       p])))

(defn parse-block [p]
  (let [[open p] (expect p :lbrace)]
    (loop [statements [] p p]
      (cond
        (at? p :rbrace)
        (let [[close p] (expect p :rbrace)
              node {:node :block :statements statements :span (span-between open close)}]
          [node p])
        (at? p :eof)
        (fail! p :parse/unexpected-token "unterminated block")
        :else
        (let [[stmt p] (parse-statement p)]
          (recur (conj statements stmt) p))))))

(defn- parse-parameters [p]
  (if (at? p :rparen)
    [[] p]
    (loop [params [] p p]
      (let [[name-t p] (expect p :identifier)
            [_ p] (expect p :colon)
            [type p] (parse-type p)
            [default p] (if (at? p :eq)
                          (parse-expression (advance p) 0)
                          [nil p])
            param {:name (:text name-t) :type type :default default :span (:span name-t)}
            params (conj params param)]
        (if (at? p :comma)
          (recur params (advance p))
          [params p])))))

(defn- parse-function [p]
  (let [start (current p)
        native? (= (:kind start) :nfn)
        p (advance p)
        [name-t p] (expect p :identifier)
        [_ p] (expect p :lparen)
        [params p] (parse-parameters p)
        [_ p] (expect p :rparen)
        [ret p] (if (at? p :arrow)
                  (parse-type (advance p))
                  [nil p])
        [body p] (parse-block p)
        node {:node :function-decl :native? native? :name (:text name-t)
              :params params :return-type ret :body body
              :span {:start (get-in start [:span :start])
                     :end (get-in body [:span :end])
                     :line (get-in start [:span :line])
                     :column (get-in start [:span :column])}}]
    (trace/emit! {:event :parse/node :kind :function-decl :name (:name node) :span (:span node)})
    [node p]))

(defn- parse-module [p]
  (let [[start p] (expect p :module)
        [name p] (parse-qualified-name p)
        [semi p] (expect p :semicolon)]
    [{:node :module :name (:segments name) :span (span-between start semi)} p]))

(defn- parse-import [p]
  (let [[start p] (expect p :import)]
    (loop [names [] p p]
      (let [[name p] (parse-qualified-name p)
            names (conj names (:segments name))]
        (if (at? p :comma)
          (recur names (advance p))
          (let [[semi p] (expect p :semicolon)]
            [{:node :import :names names :span (span-between start semi)} p]))))))

(defn- parse-declaration [p]
  (case (kind p)
    (:fn :nfn) (parse-function p)
    (:val :var :const) (parse-value-decl p)
    (fail! p :parse/unsupported-syntax
           (str "declaration kind not implemented yet: " (name (kind p))))))

(defn parse-tokens [tokens]
  (trace/with-phase :parse
    (try
      (let [p0 {:tokens tokens :pos 0}
            [module p] (parse-module p0)
            [imports p] (loop [xs [] p p]
                          (if (at? p :import)
                            (let [[x p] (parse-import p)] (recur (conj xs x) p))
                            [xs p]))
            [decls p] (loop [xs [] p p]
                        (if (at? p :eof)
                          [xs p]
                          (let [[x p] (parse-declaration p)] (recur (conj xs x) p))))
            [_ p] (expect p :eof)
            ast {:node :file :module module :imports imports :declarations decls
                 :span {:start (get-in module [:span :start])
                        :end (get-in (last tokens) [:span :end])
                        :line (get-in module [:span :line])
                        :column (get-in module [:span :column])}}]
        (trace/emit! {:event :parse/summary :declarations (count decls)})
        {:ast ast :diagnostics []})
      (catch clojure.lang.ExceptionInfo e
        {:ast nil :diagnostics [(or (:diagnostic (ex-data e))
                                    {:category :parse/internal :severity :error
                                     :message (.getMessage e)})]}))))
