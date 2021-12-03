(ns _2020.twentyfour.core
  (:require [util :refer [input]]
            [wing.core :as wing]))

(defn tile-paths [f]
  (->> f
       input
       (map (fn [line]
              (->> line
                   (re-seq #"[n|s]?[e|w]"))))))

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

(comment
  ((step "ne") {:x 2 :y 0})
  ((step "ne") {:x 2 :y 1}))

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
  (count-black-tiles "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-floor [f]
  (let [paths (tile-paths f)]
    (->> paths
         (map apply-path)
         (group-by str)
         (remove (comp even? count second))
         (map (comp first second)))))

(def adjacent-tile-positions
  (memoize
    (fn [pos]
      (->> step vals
           (map (fn [f] (f pos)))))))


(defn flip-black-tile? [black-tile-set [_pos adjs]]
  (let [black-adjs
        (->> adjs
             (filter black-tile-set)
             count
             )]
    (or
      (= black-adjs 0)
      (> black-adjs 2))))

(defn flip-adjs-map [[bt adjs]]
  (->> adjs (map (fn [p] [p bt]))))

(defn white-tiles-to-flip
  "Expects a seq of tuples from black tile to it's neighbors, like:
  [{:x x :y y [{:x x :y y}]}]"
  [black-tiles-set black-tiles-with-adjs]
  (->> black-tiles-with-adjs
       ;; filter adjs to only white neighbors
       (map (fn [[bt adjs]]
              [bt (->> adjs (remove black-tiles-set))]
              ))
       ;; flip to tuples from adj to black tile
       (mapcat flip-adjs-map)
       ;; group-by to have white tiles keys to black tile groups
       (wing/group-by first second)
       ;; remove already black tiles
       (remove (comp black-tiles-set first))
       ;; find remaining with exactly two
       (filter (comp #{2} count second))
       ;; map to just tile pos
       (map first)))

(defn day [floor]
  (let [
        black-tiles           (set floor)
        black-tiles-with-adjs (->> floor
                                   (map
                                     (fn [t]
                                       [t (adjacent-tile-positions t)])))
        to-remove
        (->> black-tiles-with-adjs (filter
                                     (partial flip-black-tile? black-tiles))
             (map first)
             (into #{}))

        to-add (white-tiles-to-flip black-tiles black-tiles-with-adjs)]
    (->> floor
         (remove to-remove)
         (concat to-add))))

(defn days [floor n]
  (->>
    (iterate day floor)
    (drop n)
    first))

(comment
  (let [f     "example.txt"
        floor (init-floor f)]
    ;; (day floor)
    (-> (days floor 100) count)
    )

  (let [f     "input.txt"
        floor (init-floor f)]
    (-> (days floor 100) count)
    )
  )
