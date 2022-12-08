(ns _2022._08.core-test
  (:require
   [_2022._08.core :as sut]
   [clojure.test :refer [deftest is]]))

(deftest test_parse_grid
  (let [grid (sut/input "example.txt")]
    (is 3 (get grid [0 0]))
    (is 3 (get grid [2 2]))
    (is 0 (get grid [4 4]))))
