(ns _2021.seven.core
  (:require [util :refer [input]]))

(defn parse [f]
  (->> f input first (re-seq #"\d+") (map read-string)))

(def example (parse "example.txt"))
(def puzz-input (parse "input.txt"))

(defn abs [v] (if (neg? v) (* -1 v) v))

(defn calc-total-fuel [example target]
  (->> example
       (map (comp abs #(- % target)))
       (apply +)))

(comment
  (calc-total-fuel example 0)
  (calc-total-fuel example 2)
  (calc-total-fuel example 16)

  (let [data  puzz-input
        min-v (apply min data)
        max-v (apply max data)]
    (->> (range min-v max-v)
         (map #(calc-total-fuel data %))
         sort
         first)))

;; part 2

(defn fuel-cost [n]
  (if (zero? n)
    0
    (+ n (fuel-cost (dec n)))))

(comment
  (fuel-cost 1)
  (fuel-cost 2)
  (fuel-cost 3)
  (fuel-cost 4))

(defn calc-total-fuel-2 [example target]
  (->> example
       (map (comp abs #(- % target)))
       (map fuel-cost)
       (apply +)))

(comment
  (calc-total-fuel-2 example 0)

  (let [data  puzz-input
        min-v (apply min data)
        max-v (apply max data)]
    (->> (range min-v max-v)
         (map #(calc-total-fuel-2 data %))
         sort
         first)))
