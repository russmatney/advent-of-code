(ns _2021.six.core
  (:require [util :refer [input]])
  )

(comment
  (input "example.txt")
  (input "input.txt")
  )

(defn initial [f]
  (->> f
       input
       first
       (re-seq #"\d" )
       (map read-string)))

(comment
  (initial "example.txt"))

(defn process-day [fish]
  (let [new-fish-ct  (->> fish (filter zero?) count)
        updated-fish (->> fish (map dec) (map (fn [n] (if (< n 0) 6 n))))
        ]
    (apply conj updated-fish (repeat new-fish-ct 8))))


(comment


  (process-day '(3 4 3 1 2))
  (process-day
    (process-day
      (process-day '(3 4 3 1 2))))


  (def day-goal 256)
  (loop [fish (initial "example.txt")
         ;; ct   (count fish)
         day  0]
    (if (#{day-goal} day)
      (count fish)
      (recur
        (process-day fish)
        (inc day))))
  )
