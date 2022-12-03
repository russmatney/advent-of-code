(ns _2022._03.core
  (:require [util :as util]
            [clojure.set :as set]))

(defn input [f]
  (util/parse-input (str "src/_2022/_03/" f)))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn ->priority [char]
  (let [i (int char)]
    (cond
      (and (<= i 90) (>= i 65)) (- i 38)
      (> i 90)                  (- i 96))))

(comment
  (- (int \A) 38)
  (- (int \Z) 38)
  (- (int \a) 96))

(defn rucksack [line]
  (let [half   (/ (count line) 2)
        comp1  (->> line (take half) (into #{}))
        comp2  (->> line (drop half) (into #{}))
        common (set/intersection comp1 comp2)]
    {:comp1    comp1
     :comp2    comp2
     :common   common
     :priority (->priority (first common))}))

(comment
  (rucksack "vJrwpWtwJgWrhcsFMMfFFhFp"))

(defn total-priority [f]
  (->> (input f)
       (map rucksack)
       (map :priority)
       (reduce +)))

(comment
  (total-priority "example.txt")
  (total-priority "input.txt"))

(defn group-badge [group]
  (->> group
       (map set)
       (apply set/intersection)
       first))

(defn total-badge-priorities [f]
  (->>
    (input f)
    (partition 3 3)
    (map group-badge)
    (map ->priority)
    (reduce +)))

(comment
  (total-badge-priorities "example.txt")
  (total-badge-priorities "input.txt"))
