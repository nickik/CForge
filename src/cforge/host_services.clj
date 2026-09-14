(ns cforge.host-services
  (:require [clojure.java.io :as io])
  (:import [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path StandardOpenOption]))

(def ^:dynamic *program-args* [])

(defn args-count [] (count *program-args*))

(defn args-get [index]
  (when (or (neg? index) (>= index (count *program-args*)))
    (throw (ex-info "argument index out of range"
                    {:diagnostic {:category :runtime/args
                                  :severity :error
                                  :message "argument index out of range"}})))
  (nth *program-args* index))

(defn- path-of ^Path [path]
  (.toPath (io/file path)))

(def default-file-provider
  {:exists? (fn [path]
              (Files/exists (path-of path) (make-array java.nio.file.LinkOption 0)))
   :read-text (fn [path]
                (let [p (path-of path)]
                  (if (Files/exists p (make-array java.nio.file.LinkOption 0))
                    (Files/readString p StandardCharsets/UTF_8)
                    "")))
   :write-text (fn [path text]
                 (Files/writeString (path-of path) text StandardCharsets/UTF_8
                                    (into-array StandardOpenOption
                                                [StandardOpenOption/CREATE
                                                 StandardOpenOption/TRUNCATE_EXISTING
                                                 StandardOpenOption/WRITE]))
                 nil)
   :append-text (fn [path text]
                  (Files/writeString (path-of path) text StandardCharsets/UTF_8
                                     (into-array StandardOpenOption
                                                 [StandardOpenOption/CREATE
                                                  StandardOpenOption/APPEND
                                                  StandardOpenOption/WRITE]))
                  nil)})

(def ^:dynamic *file-provider* default-file-provider)

(defn file-exists? [path] ((:exists? *file-provider*) path))
(defn file-read-text [path] ((:read-text *file-provider*) path))
(defn file-write-text! [path text] ((:write-text *file-provider*) path text))
(defn file-append-text! [path text] ((:append-text *file-provider*) path text))

(def default-clock-provider
  {:monotonic-us (fn [] (quot (System/nanoTime) 1000))})

(def ^:dynamic *clock-provider* default-clock-provider)
(defn monotonic-us [] ((:monotonic-us *clock-provider*)))

(def default-lock-provider
  {:acquire-exclusive
   (fn [path]
     (let [lock-path (str path ".lock")
           channel (FileChannel/open (path-of lock-path)
                                     (into-array StandardOpenOption
                                                 [StandardOpenOption/CREATE
                                                  StandardOpenOption/WRITE]))
           lock (.lock channel)]
       {:channel channel :lock lock}))
   :release
   (fn [{:keys [lock channel]}]
     (when lock (.release lock))
     (when channel (.close channel))
     nil)})

(def ^:dynamic *lock-provider* default-lock-provider)

(defn lock-acquire-exclusive [path]
  ((:acquire-exclusive *lock-provider*) path))

(defn lock-release! [token]
  ((:release *lock-provider*) token))

(defn with-exclusive-file-lock [path f]
  (let [token (lock-acquire-exclusive path)]
    (try
      (f)
      (finally
        (lock-release! token)))))

(def default-map-provider
  {:create (fn [] (atom {}))
   :put! (fn [handle key value] (swap! handle assoc key value) nil)
   :contains? (fn [handle key] (contains? @handle key))
   :get (fn [handle key] (get @handle key))
   :count (fn [handle] (count @handle))})

(def ^:dynamic *map-provider* default-map-provider)
(defn map-create [] ((:create *map-provider*)))
(defn map-put! [handle key value] ((:put! *map-provider*) handle key value))
(defn map-contains? [handle key] ((:contains? *map-provider*) handle key))
(defn map-get [handle key] ((:get *map-provider*) handle key))
(defn map-count [handle] ((:count *map-provider*) handle))
