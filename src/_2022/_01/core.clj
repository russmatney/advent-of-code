(ns _2022._01.core
  (:require [util :refer [input]]))

(defn cal-totals [data]
  (->> data
       (util/partition-by-newlines)
       (map (fn [its]
              (->> its
                   (map read-string)
                   (reduce +))))))

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
    (util/partition-by-newlines)
    (map (fn [its]
           (->> its
                (map read-string)
                (reduce +))))
    (sort >)
    (take 3)
    (reduce +)))
