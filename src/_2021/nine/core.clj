(ns _2021.nine.core
  (:require [util :refer [input]]
            [clojure.set :as set]))

(defn parse [f]
  (->> f
       input
       (map #(->> (re-seq #"\d" %) (map read-string) vec))
       vec))

(comment
  (parse "example.txt")
  (input "input.txt")
  )


(defn num-at [rows x y]
  (when-let [row (nth rows y nil)]
    (when-let [num (nth row x nil)]
      num)))

(defn neighbors [rows x y]
  (->>
    [(num-at rows x (inc y))
     (num-at rows (dec x) y)
     (num-at rows (inc x) y)
     (num-at rows x (dec y))]
    (remove nil?)))

(defn is-low-point [rows x y]
  (let [nbrs            (neighbors rows x y)
        this            (num-at rows x y)
        this-is-lowest? (->> nbrs
                             (map #(or (not %) (< this %)))
                             (every? true?))]
    (when this-is-lowest? this)))

(defn collect-low-points [f]
  (let [rows       (parse f)
        y          (count rows)
        x          (count (first rows))
        low-points (atom [])]
    (doall
      (for [i (range x)
            j (range y)]
        (when-let [num (is-low-point rows i j)]
          (swap! low-points conj num))))
    @low-points))

(comment
  (->> (collect-low-points "input.txt")
       (map inc)
       (apply +)
       )
  )

(defn collect-low-positions [f]
  (let [rows      (parse f)
        y         (count rows)
        x         (count (first rows))
        low-poses (atom [])]
    (doall
      (for [i (range x)
            j (range y)]
        (when-let [num (is-low-point rows i j)]
          (swap! low-poses conj {:num num :x i :y j}))))
    {:low-positions @low-poses
     :rows          rows}))

(defn valid-neighbor-positions [rows x y]
  (->>
    [{:x x :y (inc y)}
     {:x x :y (dec y)}
     {:x (inc x) :y y}
     {:x (dec x) :y y}]
    (filter (fn [{:keys [x y]}] (num-at rows x y)))))

(defn collect-neighbors [rows x y]
  (loop [to-check  #{{:x x :y y}}
         collected #{{:x x :y y}}]
    (if-not (seq to-check)
      collected
      (let [next     (first to-check)
            rst      (->> (rest to-check) (into #{}))
            new-nbrs (->> (valid-neighbor-positions rows (:x next) (:y next))
                          (remove (comp #{9} (fn [{:keys [x y]}]
                                               (num-at rows x y))))
                          (remove collected)
                          (into #{}))]
        (recur
          (set/union new-nbrs rst)
          (set/union new-nbrs collected))))))

(defn ->basin-size [rows {:keys [x y]}]
  (let [neighbors (collect-neighbors rows x y)]
    (count neighbors))
  )

(comment
  (let [{:keys [low-positions rows]} (collect-low-positions "example.txt")]
    (->> low-positions
         (map (partial ->basin-size rows))
         sort
         reverse
         (take 3)
         (apply *))
    )
  )
