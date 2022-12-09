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

(defn draw-tail-visits [state]
  (let [head-position (:head-position state)
        tail-position (:tail-position state)
        visits        (:tail-visits state)
        all-pos       (conj visits head-position)
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
                                   (visits [x y])           "#"
                                   :else                    ".")))
                    (apply str))
                  "\n")))
      (apply str))))

(comment
  (println
    (draw-tail-visits {:tail-visits #{[0 0] [0 1] [1 0] [-1 0] [0 -1] [-1 -1]}}))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn head-diff [dir]
  (case dir
    :right [1 0]
    :left  [-1 0]
    :up    [0 1]
    :down  [0 -1]))

(defn vec-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec-sub [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(comment
  (vec-sub [1 0] [0 1]))

(defn move-tail
  [{:keys [head-position
           tail-position
           prev-head-position]
    :as   state}]
  (let [ht-diff (vec-sub head-position tail-position)
        ht-dist (->> ht-diff (map abs) (apply max))]
    (cond
      ;; no change
      (<= ht-dist 1) state

      :else
      (-> state
          (assoc :tail-position prev-head-position)
          (update :tail-visits conj prev-head-position)))))

(defn apply-motion [state dir]
  (let [hdiff (head-diff dir)]
    (-> state
        (assoc :prev-head-position (:head-position state))
        (update :head-position vec-add hdiff)
        move-tail)))

(declare initial-state)

(comment
  (-> initial-state
      (apply-motion :right)
      (apply-motion :right)
      (apply-motion :down)
      (apply-motion :down)
      (apply-motion :down)
      (apply-motion :right)
      (apply-motion :left)
      (apply-motion :left)
      (apply-motion :left)
      (apply-motion :left)

      draw-tail-visits
      println))

(def initial-state
  {:head-position      [0 0]
   :tail-position      [0 0]
   :prev-head-position nil
   :tail-visits        #{[0 0]}})

(defn apply-motions [ms]
  (->> ms (reduce (fn [state [dir ct]]
                    (reduce
                      (fn [state _i] (apply-motion state dir))
                      state
                      (range ct)))
                  initial-state)))


(comment
  (apply-motions (motions "example.txt")))

(defn count-tail-visits [state]
  (-> state :tail-visits count))

(comment
  (-> "example.txt"
      motions
      apply-motions
      (doto (#(-> % draw-tail-visits println)))
      count-tail-visits)

  (-> "input.txt"
      motions
      apply-motions
      (doto (#(-> % draw-tail-visits println)))
      count-tail-visits)
  )
