(ns bolth-test
  (:require bolth
            [clojure.test :refer :all]))

(deftest an-failing-test
  (is (= 1 2)))

(deftest an-passing-test
  (is (= 2 2)))
