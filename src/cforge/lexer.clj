(ns cforge.lexer
  (:require [cforge.trace :as trace]))

(def keywords
  {"module" :module "import" :import "fn" :fn "nfn" :nfn
   "val" :val "var" :var "const" :const "struct" :struct
   "enum" :enum "tagged" :tagged "distinct" :distinct "type" :type
   "return" :return "tail" :tail "if" :if "else" :else "while" :while
   "for" :for "in" :in "match" :match "when" :when "defer" :defer
   "unsafe" :unsafe "with" :with "context" :context "break" :break
   "continue" :continue "true" :true "false" :false "mut" :mut})

(def multi-tokens
  {"->" :arrow "=>" :fat-arrow "==" :eq-eq "!=" :not-eq
   "<=" :lte ">=" :gte "&&" :and-and "||" :or-or
   "<<" :shl ">>" :shr "+=" :plus-eq "-=" :minus-eq
   "*=" :star-eq "/=" :slash-eq "%=" :percent-eq})

(def single-tokens
  {\( :lparen \) :rparen \{ :lbrace \} :rbrace \[ :lbracket \] :rbracket
   \; :semicolon \, :comma \. :dot \: :colon \? :question \@ :at
   \+ :plus \- :minus \* :star \/ :slash \% :percent \= :eq
   \! :bang \< :lt \> :gt \& :amp \| :pipe \^ :caret \~ :tilde \# :hash})

(defn- ident-start? [c]
  (or (Character/isLetter ^char c) (= c \_)))

(defn- ident-part? [c]
  (or (ident-start? c) (Character/isDigit ^char c)))

(defn- diagnostic [category message start end line column]
  {:category category :severity :error :message message
   :span {:start start :end end :line line :column column}})

(defn- token [kind text value start end line column]
  (cond-> {:kind kind :text text
           :span {:start start :end end :line line :column column}}
    (some? value) (assoc :value value)))

(defn lex
  "Lex Forge source into {:tokens [...] :diagnostics [...]}.
   Byte offsets are currently Java string offsets for ASCII Forge source; this will be
   upgraded to explicit UTF-8 byte offsets before Unicode identifiers become normative."
  [^String source]
  (trace/with-phase :lex
    (let [n (.length source)]
      (loop [i 0 line 1 column 1 tokens [] diagnostics []]
        (if (>= i n)
          (let [result {:tokens (conj tokens (token :eof "" nil i i line column))
                        :diagnostics diagnostics}]
            (trace/emit! {:event :lex/summary :tokens (count (:tokens result))
                          :diagnostics (count diagnostics)})
            result)
          (let [c (.charAt source i)
                c2 (when (< (inc i) n) (.substring source i (+ i 2)))]
            (cond
              (= c \newline)
              (recur (inc i) (inc line) 1 tokens diagnostics)

              (Character/isWhitespace ^char c)
              (recur (inc i) line (inc column) tokens diagnostics)

              (= c2 "//")
              (let [j (loop [j (+ i 2)]
                        (if (or (>= j n) (= (.charAt source j) \newline)) j (recur (inc j))))]
                (recur j line (+ column (- j i)) tokens diagnostics))

              (= c2 "/*")
              (loop [j (+ i 2) l line col (+ column 2)]
                (cond
                  (>= j n)
                  (recur n l col tokens
                         (conj diagnostics (diagnostic :lex/unterminated-comment
                                                       "unterminated block comment"
                                                       i n line column)))

                  (and (< (inc j) n) (= (.substring source j (+ j 2)) "*/"))
                  (recur (+ j 2) l (+ col 2) tokens diagnostics)

                  (= (.charAt source j) \newline)
                  (recur (inc j) (inc l) 1)

                  :else (recur (inc j) l (inc col))))

              (ident-start? c)
              (let [j (loop [j (inc i)]
                        (if (and (< j n) (ident-part? (.charAt source j)))
                          (recur (inc j)) j))
                    text (.substring source i j)
                    kind (get keywords text :identifier)
                    t (token kind text (when (= kind :identifier) text) i j line column)]
                (trace/emit! {:event :lex/token :kind kind :span (:span t)})
                (recur j line (+ column (- j i)) (conj tokens t) diagnostics))

              (Character/isDigit ^char c)
              (let [j (loop [j (inc i)]
                        (if (and (< j n) (Character/isDigit ^char (.charAt source j)))
                          (recur (inc j)) j))
                    has-dot? (and (< j n) (= (.charAt source j) \.)
                                  (< (inc j) n) (Character/isDigit ^char (.charAt source (inc j))))
                    k (if has-dot?
                        (loop [k (+ j 2)]
                          (if (and (< k n) (Character/isDigit ^char (.charAt source k)))
                            (recur (inc k)) k))
                        j)
                    text (.substring source i k)
                    kind (if has-dot? :float :int)
                    value (try
                            (if has-dot? (Double/parseDouble text) (bigint text))
                            (catch Exception _ nil))
                    t (token kind text value i k line column)]
                (trace/emit! {:event :lex/token :kind kind :span (:span t)})
                (recur k line (+ column (- k i)) (conj tokens t) diagnostics))

              (= c \")
              (let [[j value ok?]
                    (loop [j (inc i) out (StringBuilder.)]
                      (cond
                        (>= j n) [j (str out) false]
                        (= (.charAt source j) \") [(inc j) (str out) true]
                        (= (.charAt source j) \newline) [j (str out) false]
                        (= (.charAt source j) \\)
                        (if (>= (inc j) n)
                          [n (str out) false]
                          (let [e (.charAt source (inc j))
                                v (case e \n \newline \r \return \t \tab \" \" \\ \\ nil)]
                            (if (nil? v)
                              [j (str out) false]
                              (do (.append out ^char v) (recur (+ j 2) out)))))
                        :else (do (.append out ^char (.charAt source j))
                                  (recur (inc j) out))))
                    text (.substring source i j)]
                (if ok?
                  (let [t (token :string text value i j line column)]
                    (trace/emit! {:event :lex/token :kind :string :span (:span t)})
                    (recur j line (+ column (- j i)) (conj tokens t) diagnostics))
                  (recur (max (inc i) j) line (+ column (max 1 (- j i))) tokens
                         (conj diagnostics
                               (diagnostic :lex/invalid-string "invalid or unterminated string literal"
                                           i j line column)))))

              (contains? multi-tokens c2)
              (let [kind (get multi-tokens c2)
                    t (token kind c2 nil i (+ i 2) line column)]
                (trace/emit! {:event :lex/token :kind kind :span (:span t)})
                (recur (+ i 2) line (+ column 2) (conj tokens t) diagnostics))

              (contains? single-tokens c)
              (let [kind (get single-tokens c)
                    t (token kind (str c) nil i (inc i) line column)]
                (trace/emit! {:event :lex/token :kind kind :span (:span t)})
                (recur (inc i) line (inc column) (conj tokens t) diagnostics))

              :else
              (recur (inc i) line (inc column) tokens
                     (conj diagnostics
                           (diagnostic :lex/invalid-token
                                       (str "invalid character '" c "'")
                                       i (inc i) line column))))))))))
