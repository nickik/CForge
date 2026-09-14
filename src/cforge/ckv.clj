(ns cforge.ckv
  (:require [clojure.string :as str]
            [cforge.host-services :as host]))

(defn- invalid-field? [s]
  (or (str/includes? s "\t")
      (str/includes? s "\n")
      (str/includes? s "\r")))

(defn- validate-field! [label value]
  (when (invalid-field? value)
    (throw (ex-info (str label " contains unsupported control characters")
                    {:diagnostic {:category :ckv/invalid-field
                                  :severity :error
                                  :message (str label " may not contain tab or newline")}}))))

(defn load-database [path]
  (let [text (host/file-read-text path)
        table (host/map-create)]
    (doseq [[line-no line] (map-indexed vector (str/split-lines text))]
      (when-not (str/blank? line)
        (let [tab (.indexOf ^String line "\t")]
          (when (neg? tab)
            (throw (ex-info "malformed CKV record"
                            {:diagnostic {:category :ckv/corrupt
                                          :severity :error
                                          :message (str "malformed CKV record at line " (inc line-no))}})))
          (let [key (subs line 0 tab)
                value (subs line (inc tab))]
            (host/map-put! table key value)))))
    table))

(defn get-value [path key]
  (validate-field! "key" key)
  (host/with-exclusive-file-lock
   path
   (fn []
     (let [start (host/monotonic-us)
           table (load-database path)
           found? (host/map-contains? table key)
           value (when found? (host/map-get table key))
           elapsed (- (host/monotonic-us) start)]
       {:found? found? :value value :elapsed-us elapsed}))))

(defn set-value! [path key value]
  (validate-field! "key" key)
  (validate-field! "value" value)
  (host/with-exclusive-file-lock
   path
   (fn []
     (let [start (host/monotonic-us)
           table (load-database path)]
       ;; The initial design deliberately reloads and updates the complete logical
       ;; database on every access. Persistence is append-only for now; the most
       ;; recent record wins when the file is reloaded.
       (host/map-put! table key value)
       (host/file-append-text! path (str key "\t" value "\n"))
       {:elapsed-us (- (host/monotonic-us) start)
        :count (host/map-count table)}))))

(defn- usage []
  (str "usage: ckv --database-file FILE set KEY VALUE\n"
       "       ckv --database-file FILE get KEY\n"))

(defn run-command [args]
  (try
    (if (or (< (count args) 4) (not= "--database-file" (first args)))
      {:exit 64 :stdout "" :stderr (usage)}
      (let [path (second args)
            command (nth args 2)]
        (case command
          "set"
          (if (not= 5 (count args))
            {:exit 64 :stdout "" :stderr (usage)}
            (let [{:keys [elapsed-us]} (set-value! path (nth args 3) (nth args 4))]
              {:exit 0
               :stdout (str "access: " elapsed-us " us\n")
               :stderr ""}))

          "get"
          (if (not= 4 (count args))
            {:exit 64 :stdout "" :stderr (usage)}
            (let [{:keys [found? value elapsed-us]} (get-value path (nth args 3))]
              (if found?
                {:exit 0
                 :stdout (str value "\naccess: " elapsed-us " us\n")
                 :stderr ""}
                {:exit 2
                 :stdout (str "access: " elapsed-us " us\n")
                 :stderr "key not found\n"})))

          {:exit 64 :stdout "" :stderr (usage)})))
    (catch clojure.lang.ExceptionInfo e
      {:exit 1 :stdout "" :stderr (str (.getMessage e) "\n")
       :diagnostic (:diagnostic (ex-data e))})))
