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

; test runner results

;; holds a persistent queue or a list or a vector (perf test this)
;; of test results, for later analysis by the runner
(def ^:dynamic *test-runner-state* nil)

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
          (io.aviso.exception/write-exception *out* actual {:frame-limit 10}))
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

(defn run-all-tests
  ([] (run-all-tests #".*"))
  ([ns-re] (run-all-tests ns-re {}))
  ([ns-re options]
   (binding [*out* (java.io.PrintWriter. System/out)
             clojure.test/*test-out* (java.io.PrintWriter. System/out)
             *test-runner-state* (atom {:last-test-finished (System/nanoTime) :results []})]
     (println (apply str (repeat 50 "\n")))
     (println "running...")
     (flush)
     (let [running (atom true)
           f (future (while @running (do (Thread/sleep 10) (flush))))
           t0 (System/nanoTime)
           test-run (clojure.test/run-all-tests ns-re)
           t1 (System/nanoTime)
           results {:test-run test-run
                    :test-runner-state @*test-runner-state*
                    :runtime (float (/ (- t1 t0) 1000000))}]
       (reset! running false)
       (println "\n")
       (if (failing-tests? results)
         (println (str "\u001B[31m" (format-results results) "\u001B[m"))
         (println (str "\u001B[32m" (format-results results) "\u001B[m")))
       (when (< (:warn-on-slow-test-limit-ms options 1000) (:runtime results))
         (println (redify "SLOW TEST RUN")))
       (if (and (not (failing-tests? results))
                (:show-slow-tests options))
         (println (format-slow-tests results options)))
       results))))

(defn run-all-tests
  ([] (run-all-tests #".*"))
  ([ns-re] (run-all-tests ns-re {}))
  ([ns-re options]
   (binding [*out* (java.io.PrintWriter. System/out)
             clojure.test/*test-out* (java.io.PrintWriter. System/out)
             *test-runner-state* (atom {:last-test-finished (System/nanoTime) :results []})]
     (let [running (atom true)
           f (future (while @running (do (Thread/sleep 10) (flush))))
           t0 (System/nanoTime)
           test-run (clojure.test/run-all-tests ns-re)
           t1 (System/nanoTime)
           results {:test-run test-run
                    :test-runner-state @*test-runner-state*
                    :runtime (float (/ (- t1 t0) 1000000))}]
       (reset! running false)
       (println "\n")
       (if (failing-tests? results)
         (println (str "\u001B[31m" (format-results results) "\u001B[m"))
         (println (str "\u001B[32m" (format-results results) "\u001B[m")))
       (when (< 700 (:runtime results))
         (println (redify "SLOW TEST RUN")))
       (if (and (not (failing-tests? results))
                (:show-slow-tests options))
         (println (format-slow-tests results options)))
       results))))
