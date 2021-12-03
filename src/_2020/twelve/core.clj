(ns _2020.twelve.core
  (:require [util :refer [input]]))


(defn parse [f]
  (->> (input f)
       (map (fn [line]
              (let [[_ letter num] (->> (re-seq #"([N|S|E|W|F|L|R]{1})(\d+)" line)
                                        first)]
                {:letter letter :num (read-string num)})))))

(comment
  (parse "example.txt")
  (parse "input.txt")
  )

(def commands-part-1
  {"N" (fn [{:keys [position facing]} num]
         {:position {:x (:x position)
                     :y (+ (:y position) num)}
          :facing   facing})

   "S" (fn [{:keys [position facing]} num]
         {:position {:x (:x position)
                     :y (- (:y position) num)}
          :facing   facing})

   "E" (fn [{:keys [position facing]} num]
         {:position {:x (+ (:x position) num)
                     :y (:y position)}
          :facing   facing})

   "W" (fn [{:keys [position facing]} num]
         {:position {:x (- (:x position) num)
                     :y (:y position)}
          :facing   facing})

   "F" (fn [{:keys [facing] :as agg} num]
         {:position
          (:position (case (mod facing 360)
                       0   ((commands-part-1 "E") agg num)
                       90  ((commands-part-1 "S") agg num)
                       180 ((commands-part-1 "W") agg num)
                       270 ((commands-part-1 "N") agg num)))
          :facing facing})

   "L" (fn [{:keys [position facing]} num]
         {:position position
          :facing   (- facing num)})

   "R" (fn [{:keys [position facing]} num]
         {:position position
          :facing   (+ facing num)})})

(comment
  (mod 540 360)
  )

(defn travel
  ([f commands] (travel f commands nil))
  ([f commands waypoint]
   (->> (parse f)
        (reduce
          (fn [agg {:keys [letter num]}]
            (println agg)
            ((commands letter) agg num))
          {:position {:x 0 :y 0}
           :waypoint waypoint
           :facing   0}))))

(comment
  (->> (travel "example.txt" commands-part-1)
       :position
       vals
       (map #(Math/abs %))
       (apply +))

  (->> (travel "input.txt" commands-part-1)
       :position
       vals
       (map #(Math/abs %))
       (apply +))
  )

(defn rotate-waypoint [num waypoint]
  (case (mod num 360)
    0   waypoint
    90  {:x (:y waypoint)
         :y (* -1 (:x waypoint))}
    180 {:x (* -1 (:x waypoint))
         :y (* -1 (:y waypoint))}
    270 {:x (* -1 (:y waypoint))
         :y (:x waypoint)}))

(def commands-part-2
  {"N" (fn [{:keys [position waypoint]} num]
         {:waypoint {:x (:x waypoint)
                     :y (+ (:y waypoint) num)}
          :position position})

   "S" (fn [{:keys [position waypoint]} num]
         {:waypoint {:x (:x waypoint)
                     :y (- (:y waypoint) num)}
          :position position})

   "E" (fn [{:keys [position waypoint]} num]
         {:waypoint {:x (+ (:x waypoint) num)
                     :y (:y waypoint)}
          :position position})

   "W" (fn [{:keys [position waypoint]} num]
         {:waypoint {:x (- (:x waypoint) num)
                     :y (:y waypoint)}
          :position position})

   "F" (fn [{:keys [position waypoint]} num]
         {:position
          {:x (+ (:x position) (* num (:x waypoint)))
           :y (+ (:y position) (* num (:y waypoint)))}
          :waypoint waypoint})

   "L" (fn [{:keys [position waypoint]} num]
         {:position position
          :waypoint (rotate-waypoint (- 360 num) waypoint)})

   "R" (fn [{:keys [position waypoint]} num]
         {:position position
          :waypoint (rotate-waypoint num waypoint)})})

(comment
  (->> (travel "example.txt" commands-part-2 {:x 10 :y 1})
       :position
       vals
       (map #(Math/abs %))
       (apply +))

  (->> (travel "input.txt" commands-part-2 {:x 10 :y 1})
       :position
       vals
       (map #(Math/abs %))
       (apply +))
  ;; 51298 too low
  )
