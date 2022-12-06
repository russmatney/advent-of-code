(ns _2022._06.core
  (:require [util :as util]))

(defn input [f]
  (first (util/parse-input (str "src/_2022/_06/" f))))

(comment
  (input "example.txt")
  (input "example2.txt")
  (input "input.txt"))

(defn first-unique
  ([line] (first-unique line 4))
  ([line n]
   (loop [next   (first line)
          rst    (rest line)
          i      0
          last-n []]

     (if (#{n} (count (set last-n)))
       (do
         (println "found a winner" i)
         i)
       (recur
         (first rst)
         (rest rst)
         (inc i)
         (if (< (count last-n) n)
           (conj last-n next)
           (conj (->> last-n (drop 1) (into [])) next)))))))

(comment
  (conj (->> [1 2 3] (drop 1) (into [])) 4)
  (first-unique (input "example.txt"))
  (first-unique (input "example2.txt"))
  (first-unique (input "input.txt"))

  (first-unique (input "example.txt") 14)
  (first-unique (input "input.txt") 14))
