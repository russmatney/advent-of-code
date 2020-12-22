(ns twentytwo.core
  (:require [util :refer [input]]))

(defn decks [f]
  (->> f
       input
       (util/partition-by-newlines)
       (map (fn [hand]
              (->> hand
                   rest
                   (map read-string)
                   (into []))))))

(comment
  (decks "example.txt")
  (decks "input.txt")
  )

(defn play [f]
  (loop [[p1 p2] (decks f)]
    (if (and (seq p1) (seq p2))
      (let [p1card (first p1)
            p2card (first p2)]
        (recur
          (if (> p1card p2card)
            [(concat (rest p1) [p1card p2card]) (rest p2)]
            [(rest p1) (concat (rest p2) [p2card p1card])])))
      (if (seq p1) p1 p2))))

(comment
  (play "example.txt")
  )

(defn score [cards]
  (->> cards
       reverse
       (map-indexed (fn [i v]
                      [(inc i) v]))
       (map #(apply * %))
       (apply +)))

(comment
  (-> (play "example.txt")
      score)
  (-> (play "input.txt")
      score)
  )
