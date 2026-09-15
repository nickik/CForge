(ns cforge.builtins
  (:require [clojure.string :as str]
            [cforge.collection-storage :as storage]
            [cforge.host :as host]
            [cforge.host-services :as services]
            [cforge.sia-machine :as sia]))

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
   ["string" "from_usize"] {:import ["std" "string"] :args [:usize] :return :str :builtin :std.string/from-usize}
   ["string" "parse_u64"] {:import ["std" "string"] :args [:str] :return :u64 :builtin :std.string/parse-u64}
   ["string" "parse_usize"] {:import ["std" "string"] :args [:str] :return :usize :builtin :std.string/parse-usize}
   ["string" "byte_len"] {:import ["std" "string"] :args [:str] :return :usize :builtin :std.string/byte-len}
   ["string" "byte_at"] {:import ["std" "string"] :args [:str :usize] :return :usize :builtin :std.string/byte-at}

   ;; True architectural SIA effects. CForge models only the machine state/effect;
   ;; Cosmic policy and page-table construction remain Forge code.
   ["sia" "status_read"] {:import ["std" "machine" "sia"] :args [] :return :u32 :builtin :std.sia/status-read}
   ["sia" "status_write"] {:import ["std" "machine" "sia"] :args [:u32] :return :void :builtin :std.sia/status-write}
   ["sia" "vmctx_read"] {:import ["std" "machine" "sia"] :args [] :return :u32 :builtin :std.sia/vmctx-read}
   ["sia" "vmctx_write"] {:import ["std" "machine" "sia"] :args [:u32] :return :void :builtin :std.sia/vmctx-write}
   ["sia" "tlb_fence_all"] {:import ["std" "machine" "sia"] :args [] :return :void :builtin :std.sia/tlb-fence-all}
   ["sia" "tlb_fence_va"] {:import ["std" "machine" "sia"] :args [:u32] :return :void :builtin :std.sia/tlb-fence-va}
   ["sia" "tlb_fence_asid"] {:import ["std" "machine" "sia"] :args [:u32] :return :void :builtin :std.sia/tlb-fence-asid}

   ;; Raw typed stores are bootstrap machinery only. They support Forge-side
   ;; data structures and reference machine memory; they are not kernel APIs.
   ["raw_u8" "create"] {:import ["std" "collections" "raw_u8"] :args [:usize] :return :usize :builtin :std.raw-u8/create}
   ["raw_u8" "slots"] {:import ["std" "collections" "raw_u8"] :args [:usize] :return :usize :builtin :std.raw-u8/slots}
   ["raw_u8" "get"] {:import ["std" "collections" "raw_u8"] :args [:usize :usize] :return :u8 :builtin :std.raw-u8/get}
   ["raw_u8" "set"] {:import ["std" "collections" "raw_u8"] :args [:usize :usize :u8] :return :void :builtin :std.raw-u8/set}
   ["raw_u8" "resize"] {:import ["std" "collections" "raw_u8"] :args [:usize :usize] :return :void :builtin :std.raw-u8/resize}
   ["raw_u8" "swap"] {:import ["std" "collections" "raw_u8"] :args [:usize :usize] :return :void :builtin :std.raw-u8/swap}

   ["raw_u32" "create"] {:import ["std" "collections" "raw_u32"] :args [:usize] :return :usize :builtin :std.raw-u32/create}
   ["raw_u32" "slots"] {:import ["std" "collections" "raw_u32"] :args [:usize] :return :usize :builtin :std.raw-u32/slots}
   ["raw_u32" "get"] {:import ["std" "collections" "raw_u32"] :args [:usize :usize] :return :u32 :builtin :std.raw-u32/get}
   ["raw_u32" "set"] {:import ["std" "collections" "raw_u32"] :args [:usize :usize :u32] :return :void :builtin :std.raw-u32/set}
   ["raw_u32" "resize"] {:import ["std" "collections" "raw_u32"] :args [:usize :usize] :return :void :builtin :std.raw-u32/resize}
   ["raw_u32" "swap"] {:import ["std" "collections" "raw_u32"] :args [:usize :usize] :return :void :builtin :std.raw-u32/swap}

   ["raw_u64" "create"] {:import ["std" "collections" "raw_u64"] :args [:usize] :return :usize :builtin :std.raw-u64/create}
   ["raw_u64" "slots"] {:import ["std" "collections" "raw_u64"] :args [:usize] :return :usize :builtin :std.raw-u64/slots}
   ["raw_u64" "get"] {:import ["std" "collections" "raw_u64"] :args [:usize :usize] :return :u64 :builtin :std.raw-u64/get}
   ["raw_u64" "set"] {:import ["std" "collections" "raw_u64"] :args [:usize :usize :u64] :return :void :builtin :std.raw-u64/set}
   ["raw_u64" "resize"] {:import ["std" "collections" "raw_u64"] :args [:usize :usize] :return :void :builtin :std.raw-u64/resize}
   ["raw_u64" "swap"] {:import ["std" "collections" "raw_u64"] :args [:usize :usize] :return :void :builtin :std.raw-u64/swap}

   ["raw_usize" "create"] {:import ["std" "collections" "raw_usize"] :args [:usize] :return :usize :builtin :std.raw-usize/create}
   ["raw_usize" "slots"] {:import ["std" "collections" "raw_usize"] :args [:usize] :return :usize :builtin :std.raw-usize/slots}
   ["raw_usize" "get"] {:import ["std" "collections" "raw_usize"] :args [:usize :usize] :return :usize :builtin :std.raw-usize/get}
   ["raw_usize" "set"] {:import ["std" "collections" "raw_usize"] :args [:usize :usize :usize] :return :void :builtin :std.raw-usize/set}
   ["raw_usize" "resize"] {:import ["std" "collections" "raw_usize"] :args [:usize :usize] :return :void :builtin :std.raw-usize/resize}
   ["raw_usize" "swap"] {:import ["std" "collections" "raw_usize"] :args [:usize :usize] :return :void :builtin :std.raw-usize/swap}

   ["raw_string" "create"] {:import ["std" "collections" "raw_string"] :args [:usize] :return :usize :builtin :std.raw-string/create}
   ["raw_string" "slots"] {:import ["std" "collections" "raw_string"] :args [:usize] :return :usize :builtin :std.raw-string/slots}
   ["raw_string" "get"] {:import ["std" "collections" "raw_string"] :args [:usize :usize] :return :str :builtin :std.raw-string/get}
   ["raw_string" "set"] {:import ["std" "collections" "raw_string"] :args [:usize :usize :str] :return :void :builtin :std.raw-string/set}
   ["raw_string" "resize"] {:import ["std" "collections" "raw_string"] :args [:usize :usize] :return :void :builtin :std.raw-string/resize}
   ["raw_string" "swap"] {:import ["std" "collections" "raw_string"] :args [:usize :usize] :return :void :builtin :std.raw-string/swap}})

(defn signature-for-call [expr]
  (let [callee (:callee expr)]
    (when (and (= :member (:node callee))
               (= :name (get-in callee [:target :node])))
      (get signatures [(get-in callee [:target :name]) (:member callee)]))))

(defn resolve-call [expr imports]
  (when-let [signature (signature-for-call expr)]
    (if (contains? imports (:import signature))
      signature
      (throw (ex-info "hosted standard-library module is not imported"
                      {:diagnostic {:category :name/unresolved
                                    :severity :error
                                    :message (str "call requires import "
                                                  (str/join "." (:import signature)))
                                    :span (:span expr)}})))))

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
    :std.string/from-usize (str (first args))
    :std.string/parse-u64 (java.math.BigInteger. ^String (first args))
    :std.string/parse-usize (java.math.BigInteger. ^String (first args))
    :std.string/byte-len (count (.getBytes ^String (first args) java.nio.charset.StandardCharsets/UTF_8))
    :std.string/byte-at (let [bytes (.getBytes ^String (first args) java.nio.charset.StandardCharsets/UTF_8)]
                          (bigint (bit-and 0xff (aget bytes (int (second args))))))

    :std.sia/status-read (sia/status-read)
    :std.sia/status-write (do (sia/status-write! (first args)) nil)
    :std.sia/vmctx-read (sia/vmctx-read)
    :std.sia/vmctx-write (do (sia/vmctx-write! (first args)) nil)
    :std.sia/tlb-fence-all (do (sia/tlb-fence-all!) nil)
    :std.sia/tlb-fence-va (do (sia/tlb-fence-va! (first args)) nil)
    :std.sia/tlb-fence-asid (do (sia/tlb-fence-asid! (first args)) nil)

    :std.raw-u8/create (storage/create-u8 (first args))
    :std.raw-u8/slots (storage/slots-u8 (first args))
    :std.raw-u8/get (storage/get-u8 (nth args 0) (nth args 1))
    :std.raw-u8/set (do (storage/set-u8! (nth args 0) (nth args 1) (nth args 2)) nil)
    :std.raw-u8/resize (do (storage/resize-u8! (nth args 0) (nth args 1)) nil)
    :std.raw-u8/swap (do (storage/swap-u8! (nth args 0) (nth args 1)) nil)

    :std.raw-u32/create (storage/create-u32 (first args))
    :std.raw-u32/slots (storage/slots-u32 (first args))
    :std.raw-u32/get (storage/get-u32 (nth args 0) (nth args 1))
    :std.raw-u32/set (do (storage/set-u32! (nth args 0) (nth args 1) (nth args 2)) nil)
    :std.raw-u32/resize (do (storage/resize-u32! (nth args 0) (nth args 1)) nil)
    :std.raw-u32/swap (do (storage/swap-u32! (nth args 0) (nth args 1)) nil)

    :std.raw-u64/create (storage/create-u64 (first args))
    :std.raw-u64/slots (storage/slots-u64 (first args))
    :std.raw-u64/get (storage/get-u64 (nth args 0) (nth args 1))
    :std.raw-u64/set (do (storage/set-u64! (nth args 0) (nth args 1) (nth args 2)) nil)
    :std.raw-u64/resize (do (storage/resize-u64! (nth args 0) (nth args 1)) nil)
    :std.raw-u64/swap (do (storage/swap-u64! (nth args 0) (nth args 1)) nil)

    :std.raw-usize/create (storage/create-usize (first args))
    :std.raw-usize/slots (storage/slots-usize (first args))
    :std.raw-usize/get (storage/get-usize (nth args 0) (nth args 1))
    :std.raw-usize/set (do (storage/set-usize! (nth args 0) (nth args 1) (nth args 2)) nil)
    :std.raw-usize/resize (do (storage/resize-usize! (nth args 0) (nth args 1)) nil)
    :std.raw-usize/swap (do (storage/swap-usize! (nth args 0) (nth args 1)) nil)

    :std.raw-string/create (storage/create-string (first args))
    :std.raw-string/slots (storage/slots-string (first args))
    :std.raw-string/get (storage/get-string (nth args 0) (nth args 1))
    :std.raw-string/set (do (storage/set-string! (nth args 0) (nth args 1) (nth args 2)) nil)
    :std.raw-string/resize (do (storage/resize-string! (nth args 0) (nth args 1)) nil)
    :std.raw-string/swap (do (storage/swap-string! (nth args 0) (nth args 1)) nil)

    (throw (ex-info "unknown builtin" {:builtin builtin}))))