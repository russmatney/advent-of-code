(ns _2022._09.core
  (:require [util :as util]))

(defn path [f]
  (str "src/_2022/_09/" f))

(defn input [f]
  (util/parse-input (path f) {:split? true}))

(comment
  (input "example.txt")
  (input "input.txt"))

