(ns _2022._14.core
  (:require [util :as util]
            [clojure.set :as set]))

(defn f-path [f]
  (str "src/_2022/_14/" f))

(defn path-points [f]
  (-> f
      f-path
      (util/parse-input {:split #" -> "})
      (->> (map (fn [pts]
                  (->> pts
                       (map (fn [pt]
                              (->>
                                (re-seq #"(\d+)" pt)
                                (map second)
                                (map read-string)
                                vec)))))))))

(comment
  (path-points "example.txt")
  (path-points "input.txt"))

(defn all-path-points [f]
  (->> (path-points f)
       (reduce
         (fn [points path]
           (->>
             (partition 2 1 path)
             (mapcat (fn [[[x y] [x2 y2]]]
                       (let [sm-x (min x x2)
                             lg-x (max x x2)
                             sm-y (min y y2)
                             lg-y (max y y2)]
                         (for [px (range sm-x (inc lg-x))
                               py (range sm-y (inc lg-y))]
                           [px py]))))
             (into #{})
             (set/union points)))
         #{})))

(comment
  (all-path-points "example.txt")
  (all-path-points "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-cave [{:keys [path-points sand-start sand-points]}]
  (util/draw-grid (conj (set/union path-points sand-points) sand-start)
                  (fn [[x y]] (cond
                                (#{sand-start} [x y]) "+"
                                (sand-points [x y])   "o"
                                (path-points [x y])   "#"
                                :else                 "."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn state [f]
  (let [path-points (all-path-points f)
        max-y       (->> path-points (map second) (apply max))]
    {:path-points path-points
     :sand-start  [500, 0]
     :sand-points #{}
     :all-points  path-points
     :max-y       max-y
     :cave-floor  (+ 2 max-y)}))

(comment
  (draw-cave (state "input.txt")))

(defn sand-next-point [{:keys [all-points part-2 cave-floor]} [x y]]
  (let [down       [x (inc y)]
        down-left  [(dec x) (inc y)]
        down-right [(inc x) (inc y)]]
    (cond
      (and part-2 (= (inc y) cave-floor)) [x y]
      (not (all-points down))             down
      (not (all-points down-left))        down-left
      (not (all-points down-right))       down-right
      :else                               [x y])))

(comment
  (draw-cave (state "example.txt"))
  (sand-next-point (state "example.txt") [500 7]))

(defn add-new-sand [{:keys [part-2 sand-start] :as state}]
  (loop [sand-point sand-start]
    (let [[_x y]    sand-point
          new-point (sand-next-point state sand-point)]
      (cond
        (#{sand-start} new-point)  state
        (#{sand-point} new-point)
        (-> state
            (update :sand-points conj new-point)
            (update :all-points conj new-point))
        (and (not part-2)
             (> y (:max-y state))) state
        :else                      (recur new-point)))))

(comment
  (draw-cave (-> (state "example.txt") add-new-sand)))

(defn pour-until-stable [state]
  (->>
    state
    (iterate add-new-sand)
    (map-indexed (fn [i x] [i x]))
    (partition 2 1)
    (take-while (fn [[[_i a] [_k b]]] (not (= a b))))
    last
    second))

(comment
  (pour-until-stable (state "example.txt"))
  (pour-until-stable (state "input.txt"))

  (pour-until-stable (assoc (state "example.txt") :part-2 true))
  (pour-until-stable (assoc (state "input.txt") :part-2 true))

  (def stable-state *1)
  (-> stable-state second draw-cave))
