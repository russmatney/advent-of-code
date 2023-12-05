(ns _2023._05.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_05/" fname)
                    {:partition? true}))

(defn ->seeds [data]
  (->> data first first (re-seq #"(\d+)") (map first)))

(defn map-value [map-data val]
  (let [lowest-data (->> map-data
                         (filter #(<= (:src %) val))
                         (sort-by :src >)
                         first)]
    (cond
      (nil? lowest-data)              val
      (>= val (+ (:src lowest-data)
                 (:rng lowest-data))) val
      :else
      (let [diff (- val (:src lowest-data))]
        (+ (:dest lowest-data) diff)))))

(comment
  (->> [1 49 50 51 52 97 98 99 100]
       (map (fn [x]
              [x (map-value
                   [{:src 98 :dest 50 :rng 2}
                    {:src 50 :dest 52 :rng 2}] x)]))))

(defn ->seed-map [lines]
  (let [[src dest] (->> lines first (re-seq #"(\w+)-to-(\w+)") first rest)
        map-data   (->> lines rest (map (fn [line]
                                          (let [[dest src range]
                                                (->> line
                                                     (re-seq #"(\d+)")
                                                     (map first)
                                                     (map parse-long))]
                                            {:src src :dest dest :rng range}))))]
    {:source      src
     :destination dest
     :map-data    map-data}))

(comment
  (->> (input "example.txt") rest (first) ->seed-map))

(defn ->maps [data]
  (->> data (rest) (map ->seed-map)))

(comment
  (->maps (input "example.txt"))
  (->maps (input "input.txt")))
