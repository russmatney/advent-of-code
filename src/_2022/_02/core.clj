(ns _2022._02.core
  (:require [util :as util :refer [input]]
            [clojure.string :as string]))

(comment
  (input "example.txt")
  (input "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def choice-score
  {"X" 1
   "Y" 2
   "Z" 3})

(def win-score
  {["A" "X"] 3
   ["A" "Y"] 6
   ["A" "Z"] 0
   ["B" "X"] 0
   ["B" "Y"] 3
   ["B" "Z"] 6
   ["C" "X"] 6
   ["C" "Y"] 0
   ["C" "Z"] 3})

(defn score [them us]
  (+ (choice-score us)
     (win-score [them us])))

(comment
  (score "A" "Y")
  (score "B" "X")
  (score "C" "Z"))

(defn total [data]
  (->> data
       (map #(apply score %))
       (reduce +)))

(comment
  (total (->> (input "example.txt")
              (map #(string/split % #" "))))

  (total (->> (input "input.txt")
              (map #(string/split % #" ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def should-play
  {["A" "X"] "Z"
   ["A" "Y"] "X"
   ["A" "Z"] "Y"
   ["B" "X"] "X"
   ["B" "Y"] "Y"
   ["B" "Z"] "Z"
   ["C" "X"] "Y"
   ["C" "Y"] "Z"
   ["C" "Z"] "X"})

(defn score-2 [them outcome]
  (let [us (should-play [them outcome])]
    (score them us)))

(defn total-2 [data]
  (->> data
       (map #(apply score-2 %))
       (reduce +)))

(comment
  (total-2 (->> (input "example.txt")
                (map #(string/split % #" "))))

  (total-2 (->> (input "input.txt")
                (map #(string/split % #" ")))))
