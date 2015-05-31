(ns bolth
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]
            clojure.data
            clojure.test
            io.aviso.exception
            [bolth.state :as state]
            [clojure.tools.namespace.file :as ns-file])
  (:import java.util.concurrent.LinkedBlockingQueue
           java.util.concurrent.ArrayBlockingQueue
           java.util.AbstractQueue))

;; colour helpers
(def ^{:dynamic true
       :doc "turns on/off colored printing. You shouldn't touch this directly,
            just pass :colored true or :colored false to run-all-tests"}
  *colored-printing* true)

(defn redify [s]
  (if *colored-printing*
    (str
      "\u001B[31m"
      s
      "\u001B[m")
    s))


(defn greenify [s]
  (if *colored-printing*
    (str "\u001B[32m"
         s
         "\u001B[m")
    s))

(def ^{:dynamic true
       :doc
       "holds an (atom {}) of test results, for later analysis by the runner"}
  *test-runner-state* nil)

(def ^{:dynamic true
       :doc "because we print exceptions via clojure.test's report methods, we have to stuff these options in a dynamic var"}
  *frame-options* {:frame-limit 10})

(def ^{:dynamic true
       :doc
       "stores the results of running tests"} *result-queue* nil)

(defmacro with-test-out-str
  "Evaluates exprs in a context in which *out* and *test-out* are bound to a
  fresh StringWriter.  Returns the string created by any nested printing
  calls."
  {:added "1.0"}
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [clojure.test/*test-out* s#
               *out* s#]
       ~@body
       (str s#))))

(defmacro with-test-out [& body]
  `(let [x# (with-test-out-str ~@body)]
     (.add ^AbstractQueue *result-queue* x#)))

(defn gather-tests-from-ns [^AbstractQueue queue n]
  (let [once-fixture-fn (clojure.test/join-fixtures (::once-fixtures (meta n)))
        each-fixture-fn (clojure.test/join-fixtures (::each-fixtures (meta n)))]
    (doseq [v (vals (ns-interns n))]
      (when (:test (meta v))
        (swap! queue conj [v each-fixture-fn once-fixture-fn])))))

(defn record-test-finished [v runtime]
  (when *test-runner-state*
    (swap! *test-runner-state*
           (fn [runner-state]
             (update-in runner-state
                        [v]
                        (fn [result]
                          (assoc result
                                 :vars v
                                 :runtime runtime)))))))

(defn record-test-result [v result-type]
  (when *test-runner-state*
    (swap! *test-runner-state*
           (fn [runner-state]
             (update-in runner-state
                        [v]
                        (fn [result]
                          (assoc result
                                 :vars v
                                 :result-type result-type)))))))

(defn test-var [^AbstractQueue result-queue v]
  (when-let [t (:test (meta v))]
    (let [t0 (System/nanoTime)
          old-do-report clojure.test/do-report]
      (.setDynamic #'clojure.test/do-report)
      (binding [clojure.test/do-report (fn [m]
                                         (record-test-result v (:type m))
                                         (old-do-report m))
                clojure.test/*testing-vars* (conj clojure.test/*testing-vars* v)]
        (try
          (t)
          (catch Throwable e
            (record-test-result v :error)
            (clojure.test/do-report
              {:type :error, :message "Uncaught exception, not in assertion."
               :expected nil, :actual e}))
          (finally
            (record-test-finished v (- (System/nanoTime) t0))))))))

(defn run-worker [results ^AbstractQueue tests-to-run worker-id finished]
  (future
    (try
      (binding [*result-queue* results
                clojure.test/*report-counters* (ref {:pass 0 :fail 0 :error 0})]
        (loop []
          (if-let [[tvar each-fixture once-fixture] (.poll tests-to-run)]
            (do
              (once-fixture
                (fn []
                  (each-fixture
                    #(test-var results tvar))))
              (recur))
            (do
              (deliver (nth finished worker-id) clojure.test/*report-counters*)))))
      (catch Throwable e
        (.printStackTrace e))
      (finally (deliver (nth finished worker-id) 1)))))

(defn format-progress-result [^String r ^StringBuilder full-failures]
  (if (= r ".")
    r
    (do
      (.append full-failures r)
      (.append full-failures "\n")
      (if (.contains r "ERROR")
        (redify "E")
        (redify "F")))))

(defn start-printer [full-failures ^AbstractQueue results]
  (let [builder (StringBuilder.)]
    (future
      (loop [r (.poll results)]
        (if (= r :finished)
          (do
            (print (str builder))
            (flush))
          (do
            (if (nil? r)
              (do
                (print (str builder))
                (flush)
                (.setLength builder 0)
                (Thread/sleep 10))
              (.append builder ^String (format-progress-result r full-failures)))
            (recur (.poll results))))))))

(defn run-gathered-tests [^AbstractQueue tests-to-run pharrallelism]
  (let [results (LinkedBlockingQueue.)
        finished (into [] (map (fn [_] (promise)) (range pharrallelism)))
        workers (map #(run-worker results tests-to-run % finished) (range pharrallelism))
        full-failures (StringBuilder.)
        printer (start-printer full-failures results)]
    (try
      (dorun workers)
      (doseq [n finished]
        (deref n))
      (.put results :finished)
      @printer
      (print (str full-failures))
      (apply merge-with + (map (comp deref deref) finished))
      (finally
        (future-cancel printer)
        (doseq [worker workers]
          (future-cancel worker))))))

(defprotocol HasTests
  (gather-tests [this] "returns a vector of tests"))

(extend-type java.util.regex.Pattern
  HasTests
  (gather-tests [ns-re]
    (let [queue (atom [])]
      (dorun
        (pmap
          #(gather-tests-from-ns queue %)
          (filter #(re-matches ns-re (name (ns-name %))) (all-ns))))
      @queue)))

(defn filename->ns
  "turns a filename into an parsed clojure namespace declaration.
  returns nil if the file doesn't exist"
  [filename]
  (try
    (ns-file/read-file-ns-decl filename)
    (catch java.io.FileNotFoundException _
      nil)))

(defn parse-filename-and-line [s]
  (re-matches #"(.*):(\d+)" s))

(defn tests-from-maybe-filename [s]
  (if-let [ns-from-file (filename->ns s)]
    (gather-tests (re-pattern (str (second ns-from-file))))
    (gather-tests (re-pattern s))))

(defn test->line-number [t]
  (:line (meta (first t))))

(defn foo [line-number tests]
  (->> tests
    (group-by test->line-number)
    (sort-by first)
    last
    second))

(defn filter-tests-by-line [line-number tests]
  (->> tests
    (sort-by test->line-number)
    (take-while #(>= line-number (test->line-number %)))
    (group-by test->line-number)
    (sort-by first)
    last
    second))

(extend-type java.lang.String
  HasTests
  (gather-tests [s]
    (if-let [[_ filename line-number] (parse-filename-and-line s)]
      (filter-tests-by-line (Long/parseLong line-number) (tests-from-maybe-filename filename))
      (tests-from-maybe-filename s))))

(defn prioritize-test [[test-var _ _ :as test-run] mapped-results]
  (let [previous-result (get mapped-results (str test-var))]
    [(if (nil? previous-result) 0)
     (if (#{:error :fail} (:result-type previous-result)) (:runtime previous-result))
     (:runtime previous-result)]))

(defn slowest-test [mapped-results tests]
  (last
    (sort-by
      (fn [[test-var _ _]] (get-in mapped-results [(str test-var) :runtime]))
      tests)))

(defn smart-prioritize-tests [tests]
  (if-let [results @state/*previous-test-run*]
    (let [mapped-results (into {} (map (fn [[k v]] [(str k) v]) results))
          prioritized
          (vec
            (reverse
              (sort-by
                #(prioritize-test % mapped-results)
                tests)))
          slowest-test (slowest-test mapped-results prioritized)
          with-slowest-first (into [slowest-test] (remove #(= % slowest-test) prioritized))]
      with-slowest-first)
    tests))

(defn pour-tests-to-queue [tests]
  (let [queue (ArrayBlockingQueue. (count tests))]
    (doseq [t (remove nil? tests)]
      (.put queue t))
    queue))

(defn default-pharrallelism []
  (.. Runtime getRuntime availableProcessors))

(defn run-tests [ns-re pharrallelism prioritizer]
  (-> (gather-tests ns-re)
    prioritizer
    pour-tests-to-queue
    (run-gathered-tests pharrallelism)))

;; clojure.test monkey punching
(defmethod clojure.test/assert-expr '= [msg [_ a & more]]
       `(let [a# ~a]
          (if-let [more# (seq (list ~@more))]
            (let [result# (apply = a# more#)]
              (if result#
                (clojure.test/do-report {:type :pass, :message ~msg,
                            :expected a#, :actual more#})
                (clojure.test/do-report {:type :fail, :message ~msg,
                            :expected a#, :actual more#,
                            :diffs (map vector
                                        more#
                                        (map #(take 2 (clojure.data/diff a# %))
                                             more#))}))
              result#)
            (throw (Exception. "= expects more than one argument")))))

(defmethod clojure.test/report :begin-test-ns  [m])
(defmethod clojure.test/report :summary  [m])
(defmethod clojure.test/report :pass [m]
  (with-test-out
    (clojure.test/inc-report-counter :pass)
    (print ".")))

(defn pprint-test-failure
  "copied from humane-test-output"
  [{:keys  [type expected actual diffs message] :as event}]
  (binding [*out* (pp/get-pretty-writer clojure.test/*test-out*)]
    (let [print-expected (fn [actual]
                           (print (redify "expected: "))
                           (pp/pprint expected)
                           (print (redify "  actual: "))
                           (pp/pprint actual))]
      (if (seq diffs)
        (doseq [[actual [a b]] diffs]
          (print-expected actual)
          (when (not= a b)
          (print (redify "    diff:"))
          (if a
            (do (print " - ")
              (pp/pprint a)
              (print "          + "))
            (print " + "))
          (when b
            (pp/pprint b))))
        (print-expected actual)))))

(defn report-failure [failure-type m]
  (if (.contains ^String (clojure.test/testing-vars-str m) "EmptyList")
    (str "\n\n" (redify failure-type))
    (str "\n\n" (redify failure-type) " in " (clojure.test/testing-vars-str m))))

(defmethod clojure.test/report :fail [m]
  (with-test-out
    (clojure.test/inc-report-counter :fail)
    (println (report-failure "FAIL" m))
    (when (seq clojure.test/*testing-contexts*) (println (clojure.test/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (prn)
    (pprint-test-failure m)))

(defmethod clojure.test/report :error [m]
  (with-test-out
    (clojure.test/inc-report-counter :error)
    (println (report-failure "ERROR" m))
    (when (seq clojure.test/*testing-contexts*) (println (clojure.test/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (println "expected:" (pr-str (:expected m)))
    (print "  actual: ")
    (let [actual (:actual m)]
      (if (instance? Throwable actual)
        (do
          (prn)
          (io.aviso.exception/write-exception *out* actual *frame-options*))
        (prn actual)))))

(defn nanos->ms [nanos]
  (float (/ nanos 1000000)))

(defn format-slow-test-result [test-result]
  (str (:ns (meta (:vars test-result)))
       "/"
       (:name (meta (:vars test-result)))
       " in "
       (format "%.2f" (nanos->ms (:runtime test-result)))
       "ms"
       (if (not (empty? (:contexts test-result)))
         (str "\n   " (:contexts test-result)))))

(defn format-slow-tests [results options]
  (let [slowest (->> results
                  :test-runner-state
                  vals
                  (sort-by :runtime)
                  reverse
                  (take (:slow-test-count options 10)))]
    (str
      "slowest " (:slow-test-count options 10) " tests were:\n"
      (string/join "\n" (map-indexed (fn [n result] (str (inc n) ". " (format-slow-test-result result))) slowest))
      "\n")))


(defn failing-tests?
  "predicate - check whether a combined test.generative
  and clojure.test run failed"
  [results]
  (< 0 (+ (get-in results [:test-run :fail])
          (get-in results [:test-run :error]))))

(defn format-results
  "takes a map like
  {:test-run {:type :summary, :fail 0, :error 0, :pass 123, :test 51}}

  (which is what test results look like)
  and summarizes them into a string"
  [results]
  (let [total (+ (get-in results [:test-run :pass])
                 (get-in results [:test-run :fail])
                 (get-in results [:test-run :error]))]
    (if (zero? total)
      (redify (str "ran 0 tests, " (pr-str results)))
      (str (+ (get-in results [:test-run :pass]))
           " passes, "
           (+ (get-in results [:test-run :fail])
              (get-in results [:test-run :error]))
           " failures, "
           (+ (get-in results [:test-run :pass])
              (get-in results [:test-run :fail])
              (get-in results [:test-run :error]))
           " assertions\n"
           (format "%.2f" (:runtime results))
           "ms, "
           (format "%.2f"
                   (float (/ (:runtime results) total)))
           "ms per test"))))

(defn clear-screen [options]
  (when (:clear-screen options)
    (println (apply str (repeat (:clear-screen-lines options 50) "\n")))
    (println "running...")
    (flush)))

(defn maybe-show-slow-tests [results options]
  (if (and (not (failing-tests? results))
           (:show-slow-tests options))
    (println (format-slow-tests results options))))

(defn maybe-warn-on-slow-test-run [results options]
  (when (and (contains? options :warn-on-slow-test-limit-ms) (< (:warn-on-slow-test-limit-ms options 1000) (:runtime results)))
    (println (redify "SLOW TEST RUN"))))

(defn colorized-summary [results]
  (if (failing-tests? results)
    (redify (format-results results))
    (greenify (format-results results))))

(defn maybe-system-exit [results options]
  (if (:exit-with-error-code options false)
    (if (failing-tests? results)
      (System/exit 1)
      (System/exit 0))))

(defn options->prioritizer [options]
  (cond
    (and (nil? (:prioritizer options)) (nil? (:priority options)))
    smart-prioritize-tests

    (:prioritizer options)
    (:prioritizer options)

    (= (:priority options) :agony)
    smart-prioritize-tests

    (= (:priority options) :random)
    shuffle

    (= (:priority options) :gathered)
    identity))

(defn validate-options [options]
  (assert (and (number? (:slow-test-count options 10)) (pos? (:slow-test-count options 10))) (str ":slow-test-count should be a positive number, but was: " (pr-str (:slow-test-count options 10))))
  (assert (and (number? (:warn-on-slow-test-limit-ms options 1000)) (pos? (:warn-on-slow-test-limit-ms options 1000))) (str ":warn-on-slow-test-limit-ms should be a positive number, but was: " (pr-str (:warn-on-slow-test-limit-ms options 1000))))
  (assert (and (number? (:clear-screen-lines options 50)) (pos? (:clear-screen-lines options 50))) (str ":clear-screen-lines should be a positive number, but was: " (pr-str (:clear-screen-lines options 50))))
  (assert (and (number? (:flush-interval options 10)) (pos? (:flush-interval options 50))) (str ":flush-interval should be a positive number, but was: " (pr-str (:flush-interval options 50))))
  (assert (map? (:frame-options options *frame-options*)) (str ":frame-options should be a map, but was: " (pr-str (:frame-options options *frame-options*))))
  (assert (#{:random :agony :gathered nil} (:priority options :agony)) (str ":priority should be one of :random, :agony, :gathered, or not set at all, but was: " (pr-str (:priority options :agony))))
  (assert (fn? (options->prioritizer options)) (str "prioritizer should a function, but was: " (pr-str (options->prioritizer options)))))

(defn run-all-tests
  "runs tests, just like clojure.test/run-all-tests.
   Takes an optional regex to only run matching vars, and an options map.

  ## Available options

  :show-slow-tests : an boolean (default is false)

  tracks individual test runtimes and reports the slowest N tests (where N is specified by the option :slow-test-count, defaults to 10).
  Note that if any tests fail, the slow tests won't be shown (because you should fix the tests before optimizing test runtime).

  :slow-test-count : an positive number (default is 10)

  the number of slow tests to report (see :show-slow-tests)

  :warn-on-slow-test-limit-ms : an positive number (defaults to 1000ms which is 1 second : feedback time is an thing)

  prints a warning if the total test runtime is over the number of ms specified by this option. Feedback time is a real thing.

  :clear-screen : an boolean (defaults to false)

  if set to true, prints (:clear-screen-lines options 50) before running tests, to clear any existing screen state

  :clear-screen-lines : an positive integer (defaults to 50)

  see :clear-screen

  :force-real-stdout : an boolean (defaults to false)

  if set as a truthy value, overrides stdout to be System/out. This is useful if you're running in an editor in a tmux split
  with the repl, and triggering test runs via the editor or grenchman - it means the test output will go in the repl, so it'll
  hang around

  :flush-interval : an positive integer (defaults to 10)

  bolth prints a `.` whilst running tests, for each test run. Many test runners flush stdout after printing each `.`, but this
  can significantly slow down test suites if the suite is large enough or the terminal it's printing to is slow enough.
  Instead, bolth spawns a thread that flushes stdout every (:flush-interval options 10) milliseconds. This gives you timely
  feedback that the suite is still running, whilst not causing performance issues. After the run is finished the thread is
  cleaned up.

  :exit-with-error-code : an boolean (defaults to false)

  if set to a truthy value, calls (System/exit) after running, with exit code 0 if all the tests passed, or exit code 1 if any tests failed.

  :frame-options : an map (defaults to {:frame-limit 10})

  options for io.aviso.exception/write-exception. Defaults to just showing 10 lines from exception stacktraces. See (doc io.aviso.exception/write-exception) for full documentation.

  :colored-printing : an boolean (defaults to true)

  sets if bolth will print results with color or not. Defaults to true.

  :priority : an keyword, one of :agony, :random, :gathered (defaults to :agony)

  sets how bolth will order test runs. This interacts with parallelism in an interesting way; whilst the
  priority does set which tests get to start first, it doesn't dictate the order they finish in.
  Available options:
    :agony  : attempts to run tests in an order that gives the quickest feedback. Starts with any new tests,
              moving on to the fastest failing test, moving on to slower failing tests, then the fastest
              passing test moving on to the slowest passing test. If there was no previous test run recorded,
              this is the same as :gathered

    :random : runs tests in a random order (literally just calls `clojure.core/shuffle` on the tests

    :gathered : runs tests in the order they were found by crawling namespaces

  :prioritizer : an function (defaults to nil)

  lets you pass a custom function to determine which order the tests are run in. Takes a seq
  of [var test-f fixture-each-f fixture-once-f], and returns them in whatever order you like.
  Note that this takes priority over passing :priority.

  ## Returned value

  After running (assuming :exit-with-error-code wasn't set),
  run-all-tests returns a value detailing the test run:

  {:test-run {:type :summary, :fail 1, :error 0, :pass 1, :test 2},

  the return value from clojure.test. Returns counts of passing/failing/etc tests


  :test-runner-state
  [{:vars \"(an-failing-test) (bolth_test.clj:7)\",
      :contexts \"\",
      :result
      {:message nil,
      :actual (2),
      :expected 1,
      :diffs ([2 (1 2)]),
      :type :fail,
      :file \"bolth_test.clj\",
      :line 7},
      :runtime 37586569}],

  iff :show-slow-tests is enabled, will contain a set of test run results.

  :runtime 45.505684

  the overall time taken for this call, in milliseconds

  }"
  ([] (run-all-tests #".*"))
  ([ns-re] (run-all-tests ns-re {}))
  ([ns-re options]
   (validate-options options)
   (let [writer (if (:force-real-stdout options) (java.io.PrintWriter. System/out) *out*)]
     (binding [*out* writer
               *frame-options* (:frame-options options *frame-options*)
               clojure.test/*test-out* writer
               *test-runner-state* (atom {})
               *colored-printing* (:colored-printing options true)]
       (clear-screen options)
       (let [t0 (System/nanoTime)
             test-run (run-tests ns-re (:parallelism options (default-pharrallelism)) (options->prioritizer options))
             t1 (System/nanoTime)
             results {:test-run test-run
                      :test-runner-state (if *test-runner-state* @*test-runner-state*)
                      :runtime (float (/ (- t1 t0) 1000000))}]
         (reset! state/*previous-test-run* @*test-runner-state*)
         (println "\n")
         (println (colorized-summary results))
         (maybe-warn-on-slow-test-run results options)
         (maybe-show-slow-tests results options)
         (maybe-system-exit results options)
         results)))))

(defn pretty-refresh
  ([] (pretty-refresh [] {}))
  ([tools-ns-args frame-options]
   (require 'clojure.tools.namespace.repl)
   (require 'io.aviso.exception)
   (let [r (apply (resolve 'clojure.tools.namespace.repl/refresh) tools-ns-args)]
     (when (instance? Throwable r)
       ((resolve 'io.aviso.exception/write-exception) *out* r frame-options))
     r)))
