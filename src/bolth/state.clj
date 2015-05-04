(ns bolth.state)

(defonce
  ^{:dynamic true
    :doc "stores the results of the previous test run"}
  *previous-test-run* (atom nil))

