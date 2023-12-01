(ns _2023._01.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_01/" fname)))

(def crazy-reg #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))")

(defn str->num [s]
  (case s
    "one"   1
    "two"   2
    "three" 3
    "four"  4
    "five"  5
    "six"   6
    "seven" 7
    "eight" 8
    "nine"  9
    s))

(defn line->numbers
  ([line] (line->numbers #"(\d)" line))
  ([reg line]
   (let [matches (->> (re-seq reg line) (map second))
         f       (-> matches first str->num)
         l       (-> matches last str->num)]
     (-> (str f l) read-string))))

(comment
  (input "example.txt")
  (input "example2.txt")
  (input "input.txt")

  (re-seq #"\d" "a1b2c3d4e5f")
  (line->numbers "a1b2c3d4e5f")

  (->> (re-seq crazy-reg "ddgjgcrssevensix37twooneightgt") (map second))
  (line->numbers crazy-reg "two1nine")
  (line->numbers crazy-reg "685")
  (line->numbers crazy-reg "ddgjgcrssevensix37twooneightgt")

  ;; part 1
  (->> "input.txt" input (map line->numbers) (reduce +))

  ;; part 2
  (->> "input.txt" input (map (partial line->numbers crazy-reg)) (reduce +)))
