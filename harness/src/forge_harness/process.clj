(ns forge-harness.process
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- slurp-stream [stream]
  (with-open [r (io/reader stream)]
    (slurp r)))

(defn invoke
  "Invoke one adapter command. Returns either a parsed adapter result or a
   structured harness failure. The adapter process exit is infrastructure state;
   Forge program exit remains inside the adapter EDN payload."
  [command operation path]
  (let [argv (cond-> (vec command)
               operation (conj (name operation))
               path (conj (.getPath (io/file path))))
        pb (ProcessBuilder. ^java.util.List argv)
        process (.start pb)
        stdout-f (future (slurp-stream (.getInputStream process)))
        stderr-f (future (slurp-stream (.getErrorStream process)))
        adapter-exit (.waitFor process)
        stdout @stdout-f
        stderr @stderr-f]
    (if (zero? adapter-exit)
      (try
        {:status :ok
         :argv argv
         :adapter-exit adapter-exit
         :stderr stderr
         :raw-stdout stdout
         :result (edn/read-string stdout)}
        (catch Throwable t
          {:status :infrastructure-failure
           :category :harness/invalid-adapter-output
           :argv argv
           :adapter-exit adapter-exit
           :stdout stdout
           :stderr stderr
           :message (.getMessage t)}))
      {:status :infrastructure-failure
       :category :harness/adapter-failure
       :argv argv
       :adapter-exit adapter-exit
       :stdout stdout
       :stderr stderr})))
