(ns _2023._05.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_05/" fname)
                    {:partition? true}))

(defn ->seeds [data]
  (->> data first first (re-seq #"(\d+)") (map first)))

(comment
  (->seeds (input "input.txt")))

(defn to-map [{:keys [src dest rng]}]
  (reduce
    (fn [agg next]
      (assoc agg (+ src next) (+ dest next)))
    {}
    (range rng))
  )

(comment
  (to-map {:src 98 :dest 50 :rng 2}))

(defn ->seed-map [lines]
  (let [[src dest] (->> lines first (re-seq #"(\w+)-to-(\w+)") first rest)
        digits     (->> lines rest
                        (map (fn [line]
                               (let [[dest src range]
                                     (->> line
                                          (re-seq #"(\d+)")
                                          (map first)
                                          (map parse-long))]
                                 {:src src :dest dest :rng range}))))
        maps       (map to-map digits)
        map        (apply merge maps)]
    {:source      src
     :destination dest
     :seed-map    map}))

(comment
  (let [sm (->> (input "example.txt") rest (first) ->seed-map :seed-map)
        v  99]
    (sm v v)))

(defn ->maps [data]
  (->> data (rest) (map ->seed-map)))


(comment
  (->maps (input "example.txt")))
