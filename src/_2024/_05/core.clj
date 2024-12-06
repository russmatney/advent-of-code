(ns _2024._05.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2024/_05/" fname)
                    {:ints? true :split? true}))

(comment
  (input "example.txt"))
