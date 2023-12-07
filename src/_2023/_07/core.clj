(ns _2023._07.core
  (:require [util :as util]
            [clojure.string :as string]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_07/" fname ".txt")))

(defn parse [line]
  (let [[hand bid] (string/split line #" ")
        bid        (parse-long bid)]
    {:bid bid :raw-hand hand}))

(defn ->freqs [hand]
  (->>
    (frequencies hand)
    (map second) sort reverse
    (remove #{1})
    (into [])))

(def hand-types
  {[5]   :five
   [4]   :four
   [3 2] :full-house
   [3]   :three
   [2 2] :two-pair
   [2]   :pair
   []    :high})

(def ->type-rank
  (->> (vals hand-types)
       reverse
       (map-indexed (fn [i type] [type i]))
       (into {})))

(defn ->hand-type [hand]
  (-> hand :raw-hand ->freqs hand-types))

(def char->num
  {\1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 11
   \R 0
   \Q 12
   \K 13
   \A 14})

(defn hand->nums [raw-hand]
  (->> raw-hand (map char->num) (into [])))

(comment
  (->> ["KTJJT" "KK677"] (sort-by hand->nums))
  (->> ["QQQJA" "T55J5"] (sort-by hand->nums)))

(defn ->hand-rank [hand]
  [(-> hand ->hand-type ->type-rank)
   (-> hand :raw-hand hand->nums)])

(comment
  (->> (input "example") (map parse)
       (sort-by ->hand-rank)
       (map ->hand-rank)))

(defn total-winnings
  ([data] (total-winnings ->hand-rank data))
  ([->rank data]
   (->> data
        (map parse)
        (sort-by ->rank)
        (map-indexed (fn [rank hand] [(inc rank) hand]))
        (map (fn [[rank hand]] (* rank (:bid hand))))
        (apply +))))

(comment
  (total-winnings (input "example"))
  (total-winnings (input "input")))


;; part 2

(def joker-hand-types
  {[5]   :five
   [4]   :four
   [3 2] :full-house
   [3]   :three
   [2 2] :two-pair
   [2]   :pair
   []    :high})

(defn ->joker-freqs [hand]
  (let [freqs   (frequencies hand)
        j-count (freqs \J 0)]
    (if (#{5} j-count)
      [5]
      (->>
        freqs (#(dissoc % \J))
        (map second) sort reverse
        (into [])
        (#(update % 0 + j-count))
        (remove #{1})
        (into [])))))

(def ->joker-type-rank
  (->> (vals joker-hand-types)
       reverse
       (map-indexed (fn [i type] [type i]))
       (into {})))

(comment
  (->joker-freqs "KTJJT")
  (->joker-freqs "QQQJA")
  (->joker-freqs "QQQJ4")
  (->> (input "example")
       (map parse)
       (map #(-> % :raw-hand ->joker-freqs))))

(defn ->joker-hand-rank [hand]
  (-> hand :raw-hand ->joker-freqs
      joker-hand-types
      ->joker-type-rank))

(defn ->joker-sortable [hand]
  [(-> hand ->joker-hand-rank)
   (->> hand :raw-hand
        (map (fn [char] ({\J \R} char char)))
        hand->nums)])

(comment
  (->> (input "example")
       (map parse)
       (map ->joker-sortable))

  (total-winnings ->joker-sortable (input "example"))
  (total-winnings ->joker-sortable (input "input")))
