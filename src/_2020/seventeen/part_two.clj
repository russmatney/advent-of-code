(ns _2020.seventeen.part-two
  (:require [util :refer [input]]
            [clojure.math.combinatorics :as combo])
  )

(defn ->index-key [c]
  (str "x" (:x c) "y" (:y c) "z" (:z c) "w" (:w c)))

(defn ->indexed [c]
  [(->index-key c) c])

(defn initial-cube [f]
  (->> (input f)
       (map-indexed
         (fn [y line]
           (->> line
                (map-indexed
                  (fn [x char]
                    {:state
                     (case char
                       \. :inactive
                       \# :active)
                     :x x
                     :y y
                     :z 0
                     :w 0}))
                (into []))))
       (flatten)
       (map ->indexed)
       (into {})))

(defn active-cells [cube]
  (->> cube
       vals
       (filter (comp #{:active} :state))))

(defn ->neighbors [{:keys [x y z w] :as _c}]
  (let [xs (range (- x 1) (+ x 1 1))
        ys (range (- y 1) (+ y 1 1))
        zs (range (- z 1) (+ z 1 1))
        ws (range (- w 1) (+ w 1 1))
        ]
    (->> (combo/cartesian-product xs ys zs ws)
         (map (fn [[x y z w]]
                {:x x :y y :z z :w w}))
         (remove (fn [{cx :x cy :y cz :z cw :w}]
                   (and (= x cx)
                        (= y cy)
                        (= z cz)
                        (= w cw)))))))

(comment
  (count (->neighbors {:x -1 :y 0 :z 0 :w 0}))
  )

(defn update-cell [cube cell]
  (let [
        nbs              (->> (->neighbors cell)
                              (map (fn [c]
                                     (get cube (->index-key c)))))
        active-neighbors (->> nbs
                              (filter (comp #{:active} :state)))]
    (cond
      (= (:state cell) :active)
      (if (or (= 2 (count active-neighbors))
              (= 3 (count active-neighbors)))
        cell
        (assoc cell :state :inactive))

      (or (= (:state cell) :inactive)
          (nil? (:state cell)))
      (if (= (count active-neighbors) 3)
        (assoc cell :state :active)
        (assoc cell :state :inactive))
      ))
  )

(defn expand-cube [cube]
  (let [min-x (->> cube vals (map :x) (apply min) dec)
        max-x (->> cube vals (map :x) (apply max) inc)

        min-y (->> cube vals (map :y) (apply min) dec)
        max-y (->> cube vals (map :y) (apply max) inc)

        min-z (->> cube vals (map :z) (apply min) dec)
        max-z (->> cube vals (map :z) (apply max) inc)

        min-w (->> cube vals (map :w) (apply min) dec)
        max-w (->> cube vals (map :w) (apply max) inc)

        new-cells
        (->> (combo/cartesian-product (range min-x (+ max-x 1))
                                      (range min-y (+ max-y 1))
                                      (range min-z (+ max-z 1))
                                      (range min-w (+ max-w 1))
                                      )
             (map (fn [[x y z w]]
                    {:state :inactive :x x :y y :z z :w w}))
             (map ->indexed)
             (into {}))]
    (merge new-cells cube)))

(defn run-cycle [f n]
  (let [cube (initial-cube f)]
    (->> cube
         (iterate
           (fn [cube]
             (let [cube   (expand-cube cube)
                   active (active-cells cube)]
               (println "active-cells" (count active))
               (->> cube
                    (reduce
                      (fn [updated-cube [k cell]]
                        (let [cell
                              ;; pass cube for this iteration, not updated-cube
                              (update-cell cube cell)]
                          ;; (println "updated cell" cell)
                          (assoc updated-cube k cell)))
                      cube)))))
         (take (+ n 1))
         last)
    ))

(comment
  (run-cycle "example.txt" 6)

  (->>
    (run-cycle "example.txt" 6)
    vals
    (filter (comp #{:active} :state))
    count
    )

  (->>
    (run-cycle "input.txt" 6)
    vals
    (filter (comp #{:active} :state))
    count
    )
  )
