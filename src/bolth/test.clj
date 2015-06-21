(ns bolth.test
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]
            clojure.data
            clojure.test
            io.aviso.exception
            bolth.runner
            [bolth.state :as state]
            [clojure.tools.namespace.file :as ns-file])
  (:import java.util.concurrent.LinkedBlockingQueue
           java.util.concurrent.ArrayBlockingQueue
           java.util.AbstractQueue))

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
  ([target] (run-all-tests target {}))
  ([target options]
   (bolth.runner/run-all-tests target options)))

(defn pretty-refresh
  ([] (pretty-refresh [] {}))
  ([tools-ns-args frame-options]
   (require 'clojure.tools.namespace.repl)
   (require 'io.aviso.exception)
   (let [r (apply (resolve 'clojure.tools.namespace.repl/refresh) tools-ns-args)]
     (when (instance? Throwable r)
       ((resolve 'io.aviso.exception/write-exception) *out* r frame-options))
     r)))
