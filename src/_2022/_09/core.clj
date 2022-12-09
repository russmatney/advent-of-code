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

(comment
  (vec-sub [1 0] [0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def initial-state
  {:head-position [0 0]
   :tail-position [0 0]
   :tail-visits   #{}})

(defn state->ht-dist [{:keys [head-position tail-position]}]
  (let [ht-diff (vec-sub head-position tail-position)]
    (->> ht-diff (map abs) (apply max))))

(defn move-tail
  "Moves the tail according to the new `head-position`.
  `prev-position` is the new tail, if `head-position` is far enough away."
  [{:keys [tail-position head-position]
    :as   state}]
  (let [ht-diff   (vec-sub head-position tail-position)
        ht-dist   (->> ht-diff (map abs) (apply max))
        upd-state (update state :tail-visits conj tail-position)]
    (cond
      ;; no change
      (<= ht-dist 1) upd-state

      :else
      (let [;; isn't this the same as head-position?
            ;; new-tail-pos (vec-add ht-diff tail-position)
            new-tail-pos (:prev-head-position state)]
        (-> upd-state
            (assoc :tail-position new-tail-pos)
            ;; redundant, but fine
            (update :tail-visits conj new-tail-pos))))))

(defn move-head
  "Moves the head of a knot in the passed direction.
  May move the tail as well. See `move-tail`."
  [state dir]
  (let [hdiff (dir->diff dir)]
    (-> state
        (update :head-position vec-add hdiff)
        (assoc :prev-head-position (:head-position state))
        move-tail)))

(comment
  (-> initial-state
      (move-head :right)
      (move-head :right)
      (move-head :down)
      (move-head :down)
      (move-head :down)
      (move-head :right)
      (move-head :left)
      (move-head :left)
      (move-head :left)
      (move-head :left)

      draw-tail-visits
      println))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part one

(defn move-two-knot-rope [ms]
  (->> ms (reduce (fn [state [dir ct]]
                    (reduce
                      (fn [state _i] (move-head state dir))
                      state
                      (range ct)))
                  initial-state)))

(comment
  (move-two-knot-rope (motions "example.txt")))

(defn count-tail-visits [state]
  (-> state :tail-visits count))

(comment
  (-> "example.txt" motions move-two-knot-rope
      (doto (#(-> % draw-tail-visits println)))
      count-tail-visits)

  (-> "input.txt" motions move-two-knot-rope
      (doto (#(-> % draw-tail-visits println)))
      count-tail-visits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part two

(def ten-knot-state
  (->>
    (range 10)
    (map (fn [i]
           [i initial-state]))
    (into {})))

(defn move-first-knot [ten-state dir]
  (->> ten-state
       (sort-by first)
       (reduce (fn [t-state [i single-knot-state]]
                 (let [head-state (get t-state (dec i))
                       new-single-knot-state
                       (cond (not head-state)
                             (-> single-knot-state
                                 (assoc :prev-tail-position (:tail-position single-knot-state))
                                 (move-head dir))

                             head-state
                             (-> single-knot-state
                                 (assoc :head-position (:tail-position head-state))
                                 ;; TODO determine next tail position with new head position
                                 ;; not as sure on this part
                                 (assoc :prev-tail-position (:tail-position single-knot-state))
                                 (assoc :prev-head-position (:head-position single-knot-state))
                                 move-tail))]
                   ;; (println "knot" i)
                   ;; (println "\nhead-state" head-state)
                   ;; (println "\nnew-state" new-single-knot-state)
                   ;; (println (draw-tail-visits new-single-knot-state))
                   (assoc t-state i new-single-knot-state)))
               ten-state)))

(comment
  (-> ten-knot-state
      (move-first-knot :right)
      (move-first-knot :right)
      (move-first-knot :right)
      (move-first-knot :right)
      (move-first-knot :up)
      (move-first-knot :up)
      (move-first-knot :up)
      (move-first-knot :up)
      (move-first-knot :left)
      (move-first-knot :left)
      (move-first-knot :left)

      (get 3)
      draw-tail-visits
      println
      )
  )

(defn move-ten-knot-rope [ms]
  (->> ms
       (reduce
         (fn [state [dir ct]]
           (reduce
             (fn [state _i]
               (move-first-knot state dir))
             state
             (range ct)))
         ten-knot-state)))

(comment
  (-> "example.txt"
      motions move-ten-knot-rope
      (get 9)
      (doto (#(-> % draw-tail-visits println)))
      :tail-visits count)

  (-> "example2.txt"
      motions move-ten-knot-rope
      (get 9)
      (doto (#(-> % draw-tail-visits println)))
      :tail-visits
      count)
  )
