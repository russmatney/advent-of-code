(ns three.core
  "Given a grid like:

  ..##.......
  #...#...#..
  .#....#..#.
  ..#.#...#.#
  .#...##..#.
  ..#.##.....
  .#.#.#....#
  .#........#
  #.##...#...
  #...##....#
  .#..#...#.#

  How many `#` are encountered while traversing from 0,0 to the bottom
  along a slope of (1, 3)?
  "
  (:require [util :refer [input]]
            [clojure.string :as string]))

(defn parse [line]
  (->> line
       (map #{\#})
       (into [])))

(defn grid
  ([]
   (grid
     (string/split-lines
       "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")))
  ([input]
   (->> input
        (map parse)
        (into []))))

(defn height [g] (-> g count))

(comment
  (height (grid)))

(defn width [g] (-> g first count))

(comment
  (mod 14 (width (grid))))

(defn is-tree? [g x y]
  (let [x (mod x (width g))]
    (println (str x "," y))
    (-> g
        (nth y)
        (nth x))))

(comment
  (is-tree? (grid) 0 0)
  (is-tree? (grid) 2 0)
  (is-tree? (grid) 0 1)
  (is-tree? (grid) 6 2)
  (is-tree? (grid) 1 4)
  )

(defn weeeee! [{:keys [grid slope]}]
  (let [[dx dy] slope
        max-y   (height grid)]
    (println "max-y" max-y)
    (loop [pos   [0 0]
           trees 0]
      (let [[x y] pos
            new-x (+ x dx)
            new-y (+ y dy)]
        (if (>= new-y max-y)
          trees
          (recur [new-x new-y]
                 (if (is-tree? grid new-x new-y)
                   (inc trees)
                   trees)))))))

(comment
  (println "break")
  (weeeee! {:grid (grid) :slope [3 1]})

  (weeeee! {:grid (grid (input "input.txt")) :slope [3 1]})
  ;; 1
  ;; 15
  ;;

  )

;; PART TWO
;;

(comment
  (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]
        g      (grid (input "input.txt"))
        ;; g      (grid)
        ]
    (->> slopes
         (map #(weeeee! {:grid g :slope %}))
         (apply *)))
  )
