(ns _2022._09.core
  (:require [util :as util]))

(defn path [f]
  (str "src/_2022/_09/" f))

(defn motions [f]
  (->>
    (util/parse-input (path f) {:split? true})
    (map (fn [[dir ct]]
           [(case dir
              "R" :right
              "L" :left
              "U" :up
              "D" :down)
            (read-string ct)]))))

(comment
  (motions "example.txt")
  (motions "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw helper

(defn draw-tail-visits [{:keys [rope tail-visits]}]
  (let [head-position (first rope)
        tail-position (last rope)
        all-pos       (conj tail-visits head-position)
        max-x         (->> all-pos (map first) (apply max))
        min-x         (->> all-pos (map first) (apply min))
        max-y         (->> all-pos (map second) (apply max))
        min-y         (->> all-pos (map second) (apply min))]
    (->>
      (range min-y (inc max-y))
      reverse
      (map (fn [y]
             (str y ":\t"
                  (->>
                    (range min-x (inc max-x))
                    (map (fn [x] (cond
                                   (#{head-position} [x y]) "H"
                                   (#{tail-position} [x y]) "T"
                                   (#{[0 0]} [x y])         "s"
                                   (tail-visits [x y])      "#"
                                   :else                    ".")))
                    (apply str))
                  "\n")))
      (apply str))))

(comment
  (println
    (draw-tail-visits {:rope        [[0 0] [-1 -1]]
                       :tail-visits #{[0 0] [0 1] [1 0] [-1 0] [0 -1] [-1 -1]}}))

  ;; 15:H.....................
  ;; 14:......................
  ;; 13:......................
  ;; 12:......................
  ;; 11:......................
  ;; 10:......................
  ;; 9:	......................
  ;; 8:	......................
  ;; 7:	......................
  ;; 6:	T.....................
  ;; 5:	#.............###.....
  ;; 4:	#............#...#....
  ;; 3:	.#..........#.....#...
  ;; 2:	..#..........#.....#..
  ;; 1:	...#........#.......#.
  ;; 0:	....#......s.........#
  ;; -1:.....#..............#.
  ;; -2:......#............#..
  ;; -3:.......#..........#...
  ;; -4:........#........#....
  ;; -5:.........########.....
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dir->diff [dir]
  (case dir
    :right [1 0]
    :left  [-1 0]
    :up    [0 1]
    :down  [0 -1]))

(defn vec-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec-sub [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewrite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn next-tail-pos
  "Returns an updated `pos` relative to the passed `head-pos`"
  [pos head-pos]
  (let [ht-diff         (vec-sub head-pos pos)
        ht-dist         (->> ht-diff (map abs) (apply max))
        [diff-x diff-y] ht-diff
        [pos-x pos-y]   pos
        diff-x          (cond (pos? diff-x) 1
                              (neg? diff-x) -1
                              :else         0)
        diff-y          (cond (pos? diff-y) 1
                              (neg? diff-y) -1
                              :else         0)]
    (if (<= ht-dist 1)
      pos
      [(+ pos-x diff-x) (+ pos-y diff-y)])))

(comment
  (next-tail-pos [0 0] [1 0])
  (next-tail-pos [1 0] [3 1])
  (next-tail-pos [-1 0] [-3 -1]))

(defn pull-rope [rope dir]
  (let [diff (dir->diff dir)]
    (reduce
      (fn [new-rope knot-pos]
        (let [next-knot-pos
              (if (zero? (count new-rope))
                (vec-add diff knot-pos)
                (next-tail-pos knot-pos (peek new-rope)))]
          (into [] (concat new-rope [next-knot-pos]))))
      [] rope)))

(comment
  (-> [[0 0] [0 0] [0 0] [0 0]]
      (pull-rope :down)
      (pull-rope :right)))

(defn simulate-rope [file knots]
  (->>
    (motions file)
    (mapcat (fn [[dir ct]] (repeat ct dir)))
    (reduce
      (fn [{:keys [rope] :as state} dir]
        (let [new-rope      (pull-rope rope dir)
              tail-position (peek new-rope)]
          (-> state
              (assoc :rope new-rope)
              (update :tail-visits conj tail-position))))
      {:rope        (into [] (repeat knots [0 0]))
       :tail-visits #{[0 0]}})))

(defn count-visits [state]
  (-> state :tail-visits count))

(comment
  (simulate-rope "example.txt" 2)
  (simulate-rope "input.txt" 2)
  (simulate-rope "example2.txt" 10)
  (simulate-rope "input.txt" 10)

  (count-visits *1)

  (println (draw-tail-visits *1)))
