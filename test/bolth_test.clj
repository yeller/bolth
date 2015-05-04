(ns bolth-test
  (:require [clojure.test :refer :all]))

(deftest an-fast-failing-test
  (is (= 1 2)))

(deftest an-slow-failing-test
  (Thread/sleep 30)
  (is (= 1 2)))

(deftest an-fast-passing-test
  (is (= 2 2)))

(deftest an-slow-passing-test
  (Thread/sleep 30)
  (is (= 2 2)))

(dotimes [i 1000]
  (eval `(deftest ~(symbol (str "generated-test-" i))
           (is (= 1 1))))
  )
