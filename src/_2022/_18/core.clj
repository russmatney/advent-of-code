(ns _2022._18.core
  (:require [util :as util]))

(defn f-path [f]
  (str "src/_2022/_18/" f))

(defn input [f]
  (-> f f-path util/parse-input))

(comment
  (input "example.txt")
  (input "input.txt"))
