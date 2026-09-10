(ns cforge.trace
  (:require [clojure.pprint :as pprint]))

(def ^:dynamic *trace-sink* nil)

(defn emit!
  "Emit a structured trace event when tracing is enabled. The sink receives data,
   never preformatted text, so tests can inspect events without depending on output."
  [event]
  (when *trace-sink*
    (*trace-sink* event))
  nil)

(defn stderr-sink [pretty?]
  (fn [event]
    (binding [*out* *err*]
      (if pretty?
        (pprint/pprint event)
        (prn event)))))

(defmacro with-phase [phase & body]
  `(do
     (emit! {:event :phase/start :phase ~phase})
     (try
       (let [result# (do ~@body)]
         (emit! {:event :phase/end :phase ~phase :status :ok})
         result#)
       (catch Throwable t#
         (emit! {:event :phase/end :phase ~phase :status :error
                 :exception (.getName (class t#))})
         (throw t#)))))
