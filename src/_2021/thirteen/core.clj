(ns _2021.thirteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(defn parse-points [points]
  (->> points
       (map #(->> (string/split % #",")
                  (map read-string)
                  ((fn [[x y]] {:x x :y y}))))
       (into #{})))

(defn parse-folds [folds]
  (->> folds
       (map #(->> %
                  (re-seq #"([xy])=(\d+)") first rest
                  ((fn [[axis val]] {:axis axis :val (read-string val)}))))))

(defn parse [f]
  (->> f
       input
       util/partition-by-newlines
       ((fn [[points folds]]
          {:points (parse-points points)
           :folds  (parse-folds folds)}))))

(comment
  (parse "example.txt")
  (parse "input.txt"))

(def _ex-data (parse "example.txt"))
(def inp-data (parse "input.txt"))

(defn fold [val n]
  (if (> val n) n (- val (- n val))))

(defn apply-fold [points {:keys [axis val]}]
  (->> points
       (map (fn [{:keys [x y]}]
              (case axis
                "x" {:x (fold val x) :y y}
                "y" {:x x :y (fold val y)})))
       (into #{})))

(comment
  (apply-fold (:points ex-data) {:axis "y" :val 7}))

(comment
  (let [{:keys [points folds]} ex-data
        first-fold             (first folds)]
    (apply-fold points first-fold))

  (let [{:keys [points folds]} inp-data
        first-fold             (first folds)]
    (count (apply-fold points first-fold))))

;;

(defn do-folds [parsed]
  (let [{:keys [points folds]} parsed]
    (reduce apply-fold points folds)))

(defn grid [points]
  (let [max-x (->> points (map :x) (reduce max))
        max-y (->> points (map :y) (reduce max))]
    (for [x (range (inc max-x)) y (range (inc max-y))]
      {:x x :y y :show (if (points {:x x :y y}) "#" ".")})))

(defn pp [grid]
  (->> grid (group-by :y)
       (map (fn [[_y pts]]
              (->> pts (map :show) (apply str))))))

(comment
  (grid (do-folds ex-data))
  (pp (grid (do-folds ex-data)))
  (pp (grid (do-folds inp-data)))
  )
