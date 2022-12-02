(ns _2022._02.core-test
  (:require
   [_2022._02.core :as sut]
   [clojure.test :refer [testing deftest is]]))


(deftest score-test
  (testing "the correct score is returned"
    (is (= 8 (sut/score "A" "Y")))
    (is (= 1 (sut/score "B" "X")))
    (is (= 6 (sut/score "C" "Z")))))

(deftest score-2-test
  (testing "the correct score is returned"
    (is (= 4 (sut/score-2 "A" "Y")))
    (is (= 1 (sut/score-2 "B" "X")))
    (is (= 7 (sut/score-2 "C" "Z")))))

(deftest part1-test
  (is (= 15 (sut/total (sut/input "example.txt")))))

(deftest part2-test
  (is (= 12 (sut/total-2 (sut/input "example.txt")))))
