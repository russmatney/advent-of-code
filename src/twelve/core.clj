(ns twelve.core
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

(def commands
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
                       0   ((commands "E") agg num)
                       90  ((commands "S") agg num)
                       180 ((commands "W") agg num)
                       270 ((commands "N") agg num)))
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

(defn travel [f]
  (->> (parse f)
       (reduce
         (fn [agg {:keys [letter num]}]
           (println agg)
           ((commands letter) agg num))
         {:position {:x 0 :y 0}
          :facing   0})))

(comment
  (->> (travel "example.txt")
       :position
       vals
       (map #(Math/abs %))
       (apply +))

  (->> (travel "input.txt")
       :position
       vals
       (map #(Math/abs %))
       (apply +))
  )
