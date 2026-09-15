(ns cforge.hosted-std-forge-test
  (:require [clojure.test :refer [deftest is testing]]
            [cforge.core :as core]
            [cforge.host-services :as services]))

(deftest forge-program-uses-args-string-and-time
  (let [source (str "module test.hosted_std;\n"
                    "import std.args;\n"
                    "import std.string;\n"
                    "import std.time;\n"
                    "fn main() -> i32 {\n"
                    "  val key: str = args.get(0);\n"
                    "  val value: str = string.concat(\"v\", \"5\");\n"
                    "  val before: u64 = time.monotonic_us();\n"
                    "  val after: u64 = time.monotonic_us();\n"
                    "  if ((key == \"test\") && (value == \"v5\") && (after >= before)) { return 0; }\n"
                    "  return 1;\n"
                    "}\n")
        ticks (atom [100 105])]
    (binding [services/*program-args* ["test"]
              services/*clock-provider* {:monotonic-us (fn [] (let [v (first @ticks)]
                                                                (swap! ticks rest)
                                                                v))}]
      (let [result (core/run-source source)]
        (is (empty? (:diagnostics result)))
        (is (= 0 (:exit result)))))))

(deftest forge-program-uses-files-and-locks
  (let [file (java.io.File/createTempFile "cforge-hosted-std" ".txt")
        path (.getAbsolutePath file)
        source (str "module test.fs_lock;\n"
                    "import std.args;\n"
                    "import std.fs;\n"
                    "import std.lock;\n"
                    "fn main() -> i32 {\n"
                    "  val path: str = args.get(0);\n"
                    "  val guard: std.lock.FileLock = lock.acquire_exclusive(path);\n"
                    "  fs.write_text(path, \"alpha\");\n"
                    "  fs.append_text(path, \"beta\");\n"
                    "  val text: str = fs.read_text(path);\n"
                    "  lock.release(guard);\n"
                    "  if (text == \"alphabeta\") { return 0; }\n"
                    "  return 1;\n"
                    "}\n")]
    (try
      (binding [services/*program-args* [path]]
        (let [result (core/run-source source)]
          (is (empty? (:diagnostics result)))
          (is (= 0 (:exit result)))
          (is (= "alphabeta" (slurp file)))))
      (finally
        (.delete file)
        (.delete (java.io.File. (str path ".lock")))))))

(deftest string-line-helpers-support-database-records
  (let [source (str "module test.lines;\n"
                    "import std.string;\n"
                    "fn main() -> i32 {\n"
                    "  val text: str = \"a\\t1\\nb\\t2\\n\";\n"
                    "  val line: str = string.line_at(text, 1);\n"
                    "  if ((string.line_count(text) == 2) &&\n"
                    "      string.has_tab(line) &&\n"
                    "      (string.before_tab(line) == \"b\") &&\n"
                    "      (string.after_tab(line) == \"2\")) { return 0; }\n"
                    "  return 1;\n"
                    "}\n")
        result (core/run-source source)]
    (is (empty? (:diagnostics result)))
    (is (= 0 (:exit result)))))

(deftest raw-u32-bootstrap-storage-is-typed
  (let [source (str "module test.raw_u32;\n"
                    "import std.collections.raw_u32;\n"
                    "fn main() -> i32 {\n"
                    "  val h: usize = raw_u32.create(2);\n"
                    "  raw_u32.set(h, 0, 305419896);\n"
                    "  raw_u32.set(h, 1, 4294967295);\n"
                    "  if ((raw_u32.get(h, 0) == 305419896) &&\n"
                    "      (raw_u32.get(h, 1) == 4294967295)) { return 0; }\n"
                    "  return 1;\n"
                    "}\n")
        result (core/run-source source)]
    (is (empty? (:diagnostics result)))
    (is (= 0 (:exit result)))))