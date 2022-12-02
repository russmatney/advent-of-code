(ns _2022._01.core-test
  (:require
   [util :as util]
   [_2022._01.core :as sut]
   [clojure.test :refer [testing deftest is]]))

(deftest part-one
  (testing "example data"
    (is (= 24000 (sut/part-one (util/parse-input "src/_2022/_01/example.txt"))))))

(deftest part-two
  (testing "example data"
    (is (= 45000 (sut/part-two (util/parse-input "src/_2022/_01/example.txt"))))))
