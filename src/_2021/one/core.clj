(ns _2021.one.core
  (:require [util :refer [input]]))

(comment
  (+ 2)
  (input "example.txt")
  (input "input.txt"))

(defn count-increases [filename]
  (loop [remaining (->> (input filename) (map read-string))
         last      nil
         inc-count 0]
    (let [nxt (first remaining)]
      (if nxt
        (recur (rest remaining)
               nxt
               (if (and last (> nxt last))
                 (inc inc-count)
                 inc-count))
        inc-count))))

(comment
  (count-increases "example.txt")
  (count-increases "input.txt")
  )


(defn count-sliding-increases [filename]
  (loop [remaining (->> (input filename) (map read-string))
         last-sum  nil
         inc-count 0]
    (let [nxt-sum (when (>= (count remaining) 3)
                    (->> remaining (take 3) (reduce +)))]
      (if nxt-sum
        (recur (rest remaining)
               nxt-sum
               (if (and last-sum (> nxt-sum last-sum))
                 (inc inc-count)
                 inc-count))
        inc-count))))

(comment
  (count-sliding-increases "example.txt")
  (count-sliding-increases "input.txt")
  )
