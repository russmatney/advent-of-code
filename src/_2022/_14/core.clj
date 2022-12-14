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


(defn all-points [f]
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
  (all-points "example.txt")
  (all-points "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-cave [{:keys [path-points sand-point]}]
  (println "\n--------------------------Drawing cave-----------------")
  (let [points (conj path-points sand-point)
        max-x  (->> points (map first) (apply max))
        min-x  (->> points (map first) (apply min))
        max-y  (->> points (map second) (apply max))
        min-y  (->> points (map second) (apply min))
        rendered
        (->>
          (range min-y (inc max-y))
          (map (fn [y]
                 (str y ":\t"
                      (->>
                        (range min-x (inc max-x))
                        (map (fn [x] (cond
                                       (#{sand-point} [x y]) "+"
                                       (points [x y])        "#"
                                       :else                 ".")))
                        (apply str))
                      "\n")))
          (apply str))]
    (println rendered)
    rendered))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn state [f]
  {:path-points (all-points f)
   :sand-point  [500, 0]})

(comment
  (draw-cave (state "example.txt"))
  (draw-cave (state "input.txt")))
