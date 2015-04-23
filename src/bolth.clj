(ns bolth
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]
            clojure.data
            clojure.test
            io.aviso.exception))

;; helpers
(defn redify [s]
  (str
    "\u001B[31m"
    s
    "\u001B[m"))


(defn greenify [s]
  (str "\u001B[32m"
       s
       "\u001B[m"))

; test runner results

(def ^{:dynamic true
       :doc
       "holds an (atom []) of test results, for later analysis by the runner, iff the runner is reporting slow tests"}
  *test-runner-state* nil)

       (def ^{:dynamic true
       :doc "because we print exceptions via clojure.test's report methods, we have to stuff these options in a dynamic var"}
  *frame-options* {:frame-limit 10})

(defn record-test-finished [m]
  (when *test-runner-state*
    (let [now (System/nanoTime)]
      (swap! *test-runner-state*
             (fn [runner-state]
               {:last-test-finished now
                :results
                (conj (:results runner-state)
                      {:vars (clojure.test/testing-vars-str m)
                       :contexts (clojure.test/testing-contexts-str)
                       :result m
                       :runtime (- now (:last-test-finished runner-state))})})))))


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
  (clojure.test/with-test-out
    (record-test-finished m)
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

(defmethod clojure.test/report :fail [m]
  (clojure.test/with-test-out
    (record-test-finished m)
    (clojure.test/inc-report-counter :fail)
    (println "\n\n\u001B[31mFAIL\u001B[m in" (clojure.test/testing-vars-str m))
    (when (seq clojure.test/*testing-contexts*) (println (clojure.test/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (prn)
    (pprint-test-failure m)))

(defmethod clojure.test/report :error [m]
  (clojure.test/with-test-out
    (record-test-finished m)
    (clojure.test/inc-report-counter :error)
    (println "\n\n\u001B[31mERROR\u001B[m in" (clojure.test/testing-vars-str m))
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
  (str (string/join "" (drop 1 (take-while #(not= % \)) (:vars test-result))))
       " in "
       (format "%.2f" (nanos->ms (:runtime test-result)))
       "ms"
       (if (not (empty? (:contexts test-result)))
         (str "\n   " (:contexts test-result)))))

(defn format-slow-tests [results options]
  (let [slowest (->> results
                  :test-runner-state
                  :results
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

(defn start-flusher [interval]
  (future (if interval (while true (do (Thread/sleep interval) (flush))))))

(defn colorized-summary [results]
  (if (failing-tests? results)
    (redify (format-results results))
    (greenify (format-results results))))

(defn maybe-system-exit [results options]
  (if (:exit-with-error-code options false)
    (if (failing-tests? results)
      (System/exit 1)
      (System/exit 0))))

(defn run-all-tests
  "runs tests, just like clojure.test/run-all-tests.
   Takes an optional regex to only run matching vars, and an options map.

  Available options:
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

  options for io.aviso.exception/write-exception. Defaults to just showing 10 lines from exception stacktraces. See (doc io.aviso.exception/write-exception) for full documentation."
  ([] (run-all-tests #".*"))
  ([ns-re] (run-all-tests ns-re {}))
  ([ns-re options]
   (let [writer (if (:force-real-stdout options) (java.io.PrintWriter. System/out) *out*)]
     (binding [*out* writer
               *frame-options* (:frame-options options *frame-options*)
               clojure.test/*test-out* writer
               *test-runner-state* (if (:show-slow-tests options)
                                     (atom {:last-test-finished (System/nanoTime) :results []}))]
       (clear-screen options)
       (let [f (start-flusher (:flush-interval options 10))
             t0 (System/nanoTime)
             test-run (clojure.test/run-all-tests ns-re)
             t1 (System/nanoTime)
             results {:test-run test-run
                      :test-runner-state (if *test-runner-state* @*test-runner-state*)
                      :runtime (float (/ (- t1 t0) 1000000))}]
         (future-cancel f)
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
       ((resolve 'io.aviso.exception/write-exception) r))
     r)))
