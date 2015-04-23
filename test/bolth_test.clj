(ns bolth-test
  (:require bolth
            [clojure.test :refer :all]))

(deftest an-failing-test
  (Thread/sleep 30)
  (is (= 1 2)))

(deftest an-passing-test
  (is (= 2 2)))
