(ns _2022._19.core_test
  (:require
   [clojure.test :refer [deftest is testing]]
   [_2022._19.core :as sut]))

;; (def ex-1 (first (sut/input "example.txt")))
;; (def ex-2 (second (sut/input "example.txt")))

;; (deftest robot-build-options-test
;;   (testing "returns options for building robots"
;;     (is (= [] (sut/robot-build-options ex-1))))

;;   (testing "builds robots if it can afford to"
;;     (is (= [[:clay-robot]]
;;            (sut/robot-build-options
;;              (-> ex-1 (assoc-in [:minerals :ore] 2)))
;;            ))))
