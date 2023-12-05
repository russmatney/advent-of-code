(ns _2023._05.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_05/" fname)
                    {:partition? true}))

(defn ->seeds [data]
  (->> data first first (re-seq #"(\d+)") (map first) (map parse-long)))

(defn map-value [map-data val]
  ;; (def m map-data)
  ;; (def v v)
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
  (let [[src dest] (->> lines first (re-seq #"(\w+)-to-(\w+)") first rest (map keyword))
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

(defn ->seed-maps [data]
  (->> data (rest) (map ->seed-map)))

(comment
  (->seed-map (rest (input "example.txt")))

  (->seed-maps (input "example.txt"))
  (->seed-maps (input "input.txt")))

(defn seed->location [seed-maps seed]
  (reduce #(map-value (:map-data %2) %1) seed seed-maps))

(comment
  (-> "example.txt" input ->seed-maps)
  (-> "example.txt" input ->seed-maps (seed->location 14))
  (-> "example.txt" input ->seed-maps (seed->location 55))
  (-> "example.txt" input ->seed-maps (seed->location 13))
  )

(defn lowest-location [data]
  (let [sms (->> (rest data) (map ->seed-map))]
    (->> data ->seeds
         (map (partial seed->location sms))
         (apply min))))

(comment
  (lowest-location (input "example.txt"))
  (lowest-location (input "input.txt"))
  )
