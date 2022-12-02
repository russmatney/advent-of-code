(ns _2022._01.core-test
  (:require
   [_2022._01.core :as sut]
   [clojure.test :refer [testing deftest is]]))

(deftest part-one
  (testing "example data"
    (is (= 24000 (sut/part-one (sut/input "example.txt"))))))

(deftest part-two
  (testing "example data"
    (is (= 45000 (sut/part-two (sut/input "example.txt"))))))
