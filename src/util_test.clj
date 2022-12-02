(ns util-test
  (:require
   [util :as sut]
   [clojure.test :refer [testing deftest is]]))

(deftest parse-input
  (testing "parses basic file input"
    (is (= (list "AB" "CD" "EF" "G")
           (sut/parse-input "src/test.txt"))))

  (testing "parses lists of ints"
    (is (= (list 12 34 56 7)
           (sut/parse-input "src/test_ints.txt" {:ints? true}))))

  (testing "splits strings with spaces into vectors"
    (is (= (list ["A" "Y"] ["B" "X"] ["C" "Z"])
           (sut/parse-input "src/test_splits.txt" {:split? true}))))

  (testing "splits and parses ints"
    (is (= (list [1 2] [3 4] [5 6] [7])
           (sut/parse-input "src/test_splits_and_ints.txt" {:split? true
                                                            :ints?  true}))))
  (testing "partition by newlines"
    (is (= (list '("1" "2" "3") '("4") '("5" "6"))
           (sut/parse-input "src/test_partition_by_newlines.txt" {:partition? true}))))

  (testing "partition and parse ints"
    (is (= (list [1 2 3] [4] [5 6])
           (sut/parse-input "src/test_partition_by_newlines.txt" {:partition? true
                                                                  :ints?      true})))))
