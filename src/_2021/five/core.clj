(ns _2021.five.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(comment
  (input "example.txt")
  (input "input.txt")
  )

(defn parse-line-points [line]
  (-> line
      (string/split #" ")
      ((fn [[start _ end]]
         (let [parse-point #(->> (string/split % #",") (map read-string))
               [sx, sy]    (parse-point start)
               [ex, ey]    (parse-point end)]
           [{:x sx :y sy} {:x ex :y ey}])))))

(comment
  (->>
    (input "example.txt")
    (map parse-line-points))

  (def l "0,9 -> 5,9")
  (parse-line-points l)
  )

(defn range-between
  "Range between two numbers. INCLUSIVE! And sorted!"
  [s e]
  (if (> s e) (range e (inc s)) (range s (inc e))))

(defn gen-points [[start end]]
  (cond
    (= (:x start) (:x end))
    (->> (range-between (:y start) (:y end))
         (map (fn [y] {:y y :x (:x start)})))

    (= (:y start) (:y end))
    (->> (range-between (:x start) (:x end))
         (map (fn [x] {:x x :y (:y start)})))

    :else nil))

(comment
  (gen-points [{:x 0 :y 9} {:x 5 :y 9}])
  (gen-points [{:x 9 :y 4} {:x 3 :y 4}])
  (range 9 4)
  (range 4 9))

(comment
  (println "\n\nbreak\n")
  (->>
    (input "input.txt")
    (map parse-line-points)
    (map gen-points)
    (remove nil?)
    (apply concat)
    (group-by identity)
    ;; (map (juxt identity count))
    (map (fn [[pt pts]]
           [pt (count pts)]))
    (into {})
    (filter (fn [[_ ct]] (> ct 1)))
    (count)
    )
  )

(defn iterate-between
  "list of ints between two numbers. INCLUSIVE, and maintains the requested order"
  [s e]
  ;; I'd intended to use iterate here, but this was simple as well
  (if (> s e) (reverse (range e (inc s))) (range s (inc e))))

(comment
  (iterate-between 0 8)
  (iterate-between 8 0)
  )

(defn gen-points-2 [[start end]]
  (cond
    (= (:x start) (:x end))
    (->> (range-between (:y start) (:y end))
         (map (fn [y] {:y y :x (:x start)})))

    (= (:y start) (:y end))
    (->> (range-between (:x start) (:x end))
         (map (fn [x] {:x x :y (:y start)})))

    :else
    (let [xs (->> (iterate-between (:x start) (:x end))
                  (map (fn [x] {:x x}))
                  vec)
          ys (->> (iterate-between (:y start) (:y end))
                  (map (fn [y] {:y y}))
                  vec)]
      (->> xs (map-indexed (fn [i x] (merge x (nth ys i))))))))

(comment
  (gen-points-2 [{:x 9 :y 4} {:x 3 :y 4}])
  (gen-points-2 [{:x 0 :y 9} {:x 5 :y 9}])
  (gen-points-2 [{:x 8 :y 0} {:x 0 :y 8}])
  )

(comment
  (println "\n\nbreak\n")
  (->>
    (input "input.txt")
    (map parse-line-points)
    (map gen-points-2)
    (remove nil?)
    (apply concat)
    (group-by identity)
    ;; (map (juxt identity count))
    (map (fn [[pt pts]]
           [pt (count pts)]))
    (into {})
    (filter (fn [[_ ct]] (> ct 1)))
    (count)
    )
  )
