(ns cforge.builtins
  (:require [clojure.string :as str]
            [cforge.host :as host]
            [cforge.host-services :as services]))

(def string-map-type :std.collections.string_map.StringMap)
(def file-lock-type :std.lock.FileLock)

(def signatures
  {["console" "write"] {:import ["std" "console"] :args [:str] :return :void :builtin :std.console/write}
   ["args" "count"] {:import ["std" "args"] :args [] :return :usize :builtin :std.args/count}
   ["args" "get"] {:import ["std" "args"] :args [:usize] :return :str :builtin :std.args/get}
   ["fs" "read_text"] {:import ["std" "fs"] :args [:str] :return :str :builtin :std.fs/read-text}
   ["fs" "write_text"] {:import ["std" "fs"] :args [:str :str] :return :void :builtin :std.fs/write-text}
   ["fs" "append_text"] {:import ["std" "fs"] :args [:str :str] :return :void :builtin :std.fs/append-text}
   ["lock" "acquire_exclusive"] {:import ["std" "lock"] :args [:str] :return file-lock-type :builtin :std.lock/acquire-exclusive}
   ["lock" "release"] {:import ["std" "lock"] :args [file-lock-type] :return :void :builtin :std.lock/release}
   ["time" "monotonic_us"] {:import ["std" "time"] :args [] :return :u64 :builtin :std.time/monotonic-us}
   ["string" "concat"] {:import ["std" "string"] :args [:str :str] :return :str :builtin :std.string/concat}
   ["string" "line_count"] {:import ["std" "string"] :args [:str] :return :usize :builtin :std.string/line-count}
   ["string" "line_at"] {:import ["std" "string"] :args [:str :usize] :return :str :builtin :std.string/line-at}
   ["string" "has_tab"] {:import ["std" "string"] :args [:str] :return :bool :builtin :std.string/has-tab}
   ["string" "before_tab"] {:import ["std" "string"] :args [:str] :return :str :builtin :std.string/before-tab}
   ["string" "after_tab"] {:import ["std" "string"] :args [:str] :return :str :builtin :std.string/after-tab}
   ["string" "from_u64"] {:import ["std" "string"] :args [:u64] :return :str :builtin :std.string/from-u64}
   ["string_map" "create"] {:import ["std" "collections" "string_map"] :args [] :return string-map-type :builtin :std.string-map/create}
   ["string_map" "put"] {:import ["std" "collections" "string_map"] :args [string-map-type :str :str] :return :void :builtin :std.string-map/put}
   ["string_map" "contains"] {:import ["std" "collections" "string_map"] :args [string-map-type :str] :return :bool :builtin :std.string-map/contains}
   ["string_map" "get"] {:import ["std" "collections" "string_map"] :args [string-map-type :str] :return :str :builtin :std.string-map/get}
   ["string_map" "count"] {:import ["std" "collections" "string_map"] :args [string-map-type] :return :usize :builtin :std.string-map/count}})

(defn resolve-call [expr imports]
  (let [callee (:callee expr)]
    (when (and (= :member (:node callee))
               (= :name (get-in callee [:target :node])))
      (when-let [signature (get signatures [(get-in callee [:target :name]) (:member callee)])]
        (when (contains? imports (:import signature))
          signature)))))

(defn- lines [text]
  (if (empty? text) [] (str/split-lines text)))

(defn invoke [builtin args]
  (case builtin
    :std.console/write (do (host/console-write! (first args)) nil)
    :std.args/count (services/args-count)
    :std.args/get (services/args-get (int (first args)))
    :std.fs/read-text (services/file-read-text (first args))
    :std.fs/write-text (do (services/file-write-text! (first args) (second args)) nil)
    :std.fs/append-text (do (services/file-append-text! (first args) (second args)) nil)
    :std.lock/acquire-exclusive (services/lock-acquire-exclusive (first args))
    :std.lock/release (do (services/lock-release! (first args)) nil)
    :std.time/monotonic-us (services/monotonic-us)
    :std.string/concat (str (first args) (second args))
    :std.string/line-count (count (lines (first args)))
    :std.string/line-at (nth (lines (first args)) (int (second args)))
    :std.string/has-tab (not (neg? (.indexOf ^String (first args) "\t")))
    :std.string/before-tab (let [s ^String (first args) i (.indexOf s "\t")]
                             (if (neg? i) s (.substring s 0 i)))
    :std.string/after-tab (let [s ^String (first args) i (.indexOf s "\t")]
                            (if (neg? i) "" (.substring s (inc i))))
    :std.string/from-u64 (str (first args))
    :std.string-map/create (services/map-create)
    :std.string-map/put (do (services/map-put! (nth args 0) (nth args 1) (nth args 2)) nil)
    :std.string-map/contains (services/map-contains? (nth args 0) (nth args 1))
    :std.string-map/get (services/map-get (nth args 0) (nth args 1))
    :std.string-map/count (services/map-count (first args))
    (throw (ex-info "unknown builtin" {:builtin builtin}))))
