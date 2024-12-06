(ns _2024._03.core
  (:require [util :as util]))

(defn input [fname]
  (->>
    (util/parse-input (str "src/_2024/_03/" fname))
    (apply str)))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn parse-ops [s]
  (re-seq #"(mul)\((\d{1,3}),(\d{1,3})\)" s))

(comment
  (->> (input "input.txt") parse-ops)

  (parse-ops "xmul(2,4)%&")
  (->> "xmul(2,4)%&"
       (re-seq #"mul\(\d{1,3},\d{1,3}\)"))
  (->> "s34s899s9s" (re-seq #"\d{1,3}")))

(defn calc-op [op]
  (let [[_match cmd arg1 arg2] op]
    (case cmd
      "mul" (* (read-string arg1) (read-string arg2)))))

(comment
  (->> (input "example.txt") parse-ops (map calc-op) (reduce + 0))
  (->> (input "input.txt") parse-ops
       (map calc-op) (reduce + 0))
  166630675

  )
