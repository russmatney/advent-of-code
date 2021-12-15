(ns _2021.fifteen.core
  (:require
   [grid]
   [util :refer [input]]))

(defn parse [f]
  (->> f input grid/parse-digit-grid))

(def ex (parse "example.txt"))
(def inp (parse "input.txt"))

(comment
  )
