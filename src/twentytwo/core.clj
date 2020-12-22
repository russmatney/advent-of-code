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
      score))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn play-recursive [decks]
  (loop [decks   decks
         history {}
         round   0]
    (let [[p1 p2] decks]
      (cond
        (history decks)
        {:winner :p1
         :hand   p1}

        (and (seq p1) (seq p2))
        (let [p1card (first p1)
              p2card (first p2)
              winner (cond
                       (and
                         (< p1card (count p1))
                         (< p2card (count p2)))
                       (:winner (play-recursive [(take p1card (rest p1))
                                                 (take p2card (rest p2))]))

                       (> p1card p2card) :p1
                       :else             :p2)]
          (recur
            (case winner
              :p1 [(concat (rest p1) [p1card p2card]) (rest p2)]
              :p2 [(rest p1) (concat (rest p2) [p2card p1card])])
            (assoc history decks true)
            (inc round)))

        :else
        (if (seq p1)
          {:winner :p1
           :hand   p1}
          {:winner :p2
           :hand   p2})))))

(comment
  (def --history #{})
  (def --decks (decks "example.txt"))

  (let [h (conj --history --decks)
        h (conj h (->> --decks (into [])))]
    (h (list [1 2 3] [4 8 5]))
    )

  (println "NEWGAME")

  (-> (decks "example.txt")
      play-recursive
      :hand
      score
      )

  (-> (decks "input.txt")
      play-recursive
      :hand
      score
      )
  )
