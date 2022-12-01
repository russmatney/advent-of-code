(ns _2022._01.core-test
  (:require
   [util :refer [input]]
   [_2022._01.core :as sut]
   [clojure.test :refer [testing deftest is]]))

(deftest part-one
  (testing "example data"
    (is (= 24000 (sut/part-one (input "example.txt"))))))

(deftest part-two
  (testing "example data"
    (is (= 45000 (sut/part-two (input "example.txt"))))))
