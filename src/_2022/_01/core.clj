(ns _2022._01.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2022/_01/" fname) {:partition? true :ints? true}))

(defn cal-totals [data]
  (->> data (map #(reduce + %))))

(defn part-one [data]
  (apply max (cal-totals data)))

(defn part-two [data]
  (->>
    (cal-totals data)
    (sort >)
    (take 3)
    (reduce +)))

(comment
  (part-one (input "example.txt"))
  (part-one (input "input.txt"))

  (part-two (input "example.txt"))
  (part-two (input "input.txt"))

  (->>
    (input "example.txt")
    (map #(reduce + %))
    (sort >)
    (take 3)
    (reduce +)))
