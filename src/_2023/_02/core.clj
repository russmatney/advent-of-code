(ns _2023._02.core
  (:require [util :as util]
            [clojure.string :as string]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_02/" fname)))

(comment
  (input "input.txt"))

(defn line->game [line]
  (let [id (->> line (re-seq #"Game (\d+):") (map second) first read-string)
        hands
        (-> line
            (string/split #": ")
            second
            (string/split #"; ")
            (->>
              (map (fn [color-counts]
                     (-> color-counts
                         (string/split #", ")
                         (->> (map (fn [n-color]
                                     (let [n     (->>
                                                   (re-seq #"(\d+)" n-color)
                                                   (map second) first read-string)
                                           color (-> (string/split n-color #" ")
                                                     second keyword)]
                                       {:n n :color color})))
                              (into [])))))
              (into [])))]
    {:id    id
     :hands hands}))

(comment
  (->> (input "input.txt")
       (take 3)
       (map line->game)))

(defn game->min-colors [game]
  (reduce
    (fn [{:as agg :keys [green red blue]} color-counts]
      (reduce (fn [agg {:keys [n color]}]
                (cond-> agg
                  (and (#{:green} color) (> n green))
                  (assoc :green n)
                  (and (#{:red} color) (> n red))
                  (assoc :red n)
                  (and (#{:blue} color) (> n blue))
                  (assoc :blue n)))
              agg color-counts))
    {:green 0 :blue 0 :red 0}
    (:hands game)))

(comment
  (->> (input "example.txt")
       #_(take 3)
       (map line->game)
       (map game->min-colors)))

(defn game-is-possible? [limits game]
  (let [{:keys [max-red max-green max-blue]} limits
        {:keys [red green blue]}             (game->min-colors game)]
    (and
      (<= red max-red)
      (<= blue max-blue)
      (<= green max-green))))

(comment
  (let [limits {:max-red 12 :max-green 13 :max-blue 14}]
    (->> (input "input.txt")
         (map line->game)
         (filter (partial game-is-possible? limits))
         (map :id)
         (reduce + 0)))

  (->> (input "input.txt")
       (map line->game)
       (map game->min-colors)
       (map vals)
       (map #(apply * %))
       (reduce + 0)))
