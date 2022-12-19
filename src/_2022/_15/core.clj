(ns _2022._15.core
  (:require [util :as util]))

(defn f-path [f]
  (str "src/_2022/_15/" f))

(defn input [f]
  (-> f
      f-path
      (util/parse-input)
      (->>
        (map (fn [line]
               (let [parsed
                     (re-seq #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" line)
                     [sx sy bx by] (->> parsed first rest (map read-string))]
                 {:sensor [sx sy]
                  :beacon [bx by]}))))))

(comment
  (->>
    (input "example.txt")
    (mapcat vals)
    (into #{})
    )
  (count
    (input "input.txt")))

(defn state [f]
  (let [inp (input f)]
    {:input      inp
     :all-points (->> inp (mapcat vals) (into #{}))
     :sensors    (->> inp (map :sensor) (into #{}))
     :beacons    (->> inp (map :beacon) (into #{}))}))


(defn draw-graph [state]
  (util/draw-grid
    (:all-points state)
    (fn [[x y]]
      (cond
        ((:sensors state) [x y]) "S"
        ((:beacons state) [x y]) "B"
        :else                    "."))))

(comment
  (draw-graph (state "example.txt"))
  (draw-graph (state "input.txt")))
