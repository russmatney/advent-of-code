(ns _2024._04.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2024/_04/" fname)
                    {:ints? true :split? true}))

(comment
  (input "example.txt"))
