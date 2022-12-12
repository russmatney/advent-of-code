(ns _2022._12.core-test
  (:require
   [_2022._12.core :as sut]
   [clojure.test :refer [deftest is]]))

(deftest next-steps-test
  (let [example (sut/input "example.txt")
        init    (sut/init-path example)]
    (is (= (list [1 0] [0 1])
           (sut/neighbors example init)))))

(deftest find-shortest-path-test
  (let [result (sut/find-shortest-path "example.txt")]
    (is result)))
