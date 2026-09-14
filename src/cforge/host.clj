(ns cforge.host)

(defn- unix-write! [text]
  (print text)
  (flush)
  nil)

(def default-console-provider
  {:write unix-write!})

(def ^:dynamic *console-provider* default-console-provider)

(defn console-write! [text]
  (if-let [write-fn (:write *console-provider*)]
    (write-fn text)
    (throw (ex-info "console provider has no write operation"
                    {:diagnostic {:category :runtime/console
                                  :severity :error
                                  :message "console provider has no write operation"}}))))