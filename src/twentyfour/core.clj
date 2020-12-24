(ns twentyfour.core
  (:require [util :refer [input]]))

(defn tile-paths [f]
  (->> f
       input
       (map (fn [line]
              (->> line
                   (re-seq #"[n|s]?[e|w]"))))))

(comment
  (tile-paths "example.txt"))

(def step
  ;; even y means first row
  {"w"  (fn [{:keys [x y]}]
          {:x (dec x)
           :y y})
   "nw" (fn [{:keys [x y]}]
          {:x (if (even? y) (dec x) x)
           :y (dec y)})
   "sw" (fn [{:keys [x y]}]
          {:x (if (even? y) (dec x) x)
           :y (inc y)})
   "e"  (fn [{:keys [x y]}]
          {:x (inc x)
           :y y})
   "ne" (fn [{:keys [x y]}]
          {:x (if (even? y) x (inc x))
           :y (dec y)})
   "se" (fn [{:keys [x y]}]
          {:x (if (even? y) x (inc x))
           :y (inc y)})})

(defn apply-path [steps]
  (loop [pos   {:x 0 :y 0}
         steps steps
         ]
    (if (seq steps)
      (let [next (first steps)]
        (recur ((step next) pos) (rest steps)))
      pos)))

(defn count-black-tiles [f]
  (let [paths (tile-paths f)]
    (->> paths
         (map apply-path)
         (group-by str)
         (remove (comp even? count second))
         count
         )
    ))

(comment
  (count-black-tiles "example.txt")
  (count-black-tiles "input.txt")

  )
