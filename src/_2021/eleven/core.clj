(ns _2021.eleven.core
  (:require [util :refer [input]]
            [clojure.set :as set]))


(defn parse [f]
  (->> f input
       (map #(->> % (re-seq #"\d") (map read-string) vec))
       vec))

(def example-rows (parse "example.txt"))
(def input-rows (parse "input.txt"))

(defn num-at [rows {:keys [x y]}]
  (when-let [row (nth rows y nil)]
    (when-let [num (nth row x nil)]
      num)))

(defn set-at [rows {:keys [x y]} f]
  (update rows y #(update % x (fn [v] (f v)))))

(comment
  (set-at example-rows {:x 0 :y 0} (fn [_] "wut"))
  (set-at example-rows {:x 1 :y 1} inc))


(defn valid-neighbors [rows {:keys [x y]}]
  (for [nx    (range (dec x) (inc (inc x)))
        ny    (range (dec y) (inc (inc y)))
        :when (and (num-at rows {:x nx :y ny})
                   ;; exclude the passed coord
                   (not (and (#{nx} x) (#{ny} y))))]
    {:x nx :y ny}))

(comment
  (valid-neighbors example-rows {:x 0 :y 0})
  (valid-neighbors example-rows {:x 1 :y 1})
  (valid-neighbors example-rows {:x 2 :y 3}))

(defn all-coords [rows]
  (for [x     (range (count rows))
        y     (range (count (first rows)))
        :when (num-at rows {:x x :y y})]
    {:x x :y y}))

(defn inc-at
  "Increases the val at a coord, spreading flashes to neighbors when applicable.
  Returns the updated rows and the number of flashes."
  ([rows coord] (inc-at rows coord #{}))
  ([rows coord flashed]
   (let [val (num-at rows coord)]
     (if-not (#{9} val)
       {:rows    (if (flashed coord)
                   rows
                   (set-at rows coord inc))
        :flashes 0
        :flashed flashed}

       ;; flashing!
       (let [neighbors (valid-neighbors rows coord)
             rows      (set-at rows coord (constantly 0))]
         ;; TODO make sure 'flashed' coords don't get incremented above 0
         (reduce
           (fn [{:keys [rows flashes flashed]} n-coord]
             (let [new-acc (inc-at rows n-coord flashed)]
               ;; keep the new rows, acc the new flashes
               (update new-acc :flashes + flashes)))
           {:rows    rows
            ;; count this flash
            :flashes 1
            :flashed (set/union flashed #{coord})}
           neighbors))))))


(comment
  (inc-at
    (:rows
     (inc-at example-rows {:x 2 :y 0}))
    {:x 2 :y 0}))

(defn step [{:keys [rows flashes n]}]
  (println "step-n" n)
  (loop [acc-flashes      (or flashes 0)
         rows             rows
         [coord & coords] (all-coords example-rows)
         flashed          #{}]
    (if-not coord
      {:rows         rows
       :flashes      acc-flashes
       :all-flashed? (#{100} (count flashed))
       :n            (inc n)}
      (let [{:keys [rows flashes flashed]} (inc-at rows coord flashed)]
        (recur (+ flashes acc-flashes)
               rows
               (->> coords (remove flashed))
               flashed)))))

(defn steps [rows n]
  (->>
    (iterate step {:rows rows :n 0})
    (take (inc n))
    (last)))

(defn pp [{:keys [rows flashes]}]
  (println "Flashes:" flashes)
  (doall (->> rows (map println))))

(comment
  (pp (steps example-rows 1))
  (pp (steps example-rows 2))
  (pp (steps example-rows 10))
  (pp (steps example-rows 100))
  (pp (steps input-rows 100))
  )


(defn step-until-all-flashed [rows]
  (->>
    (iterate step {:rows rows :n 1})
    (take-while (comp not :all-flashed?))
    (last)))

(comment
  (step-until-all-flashed example-rows)
  (step-until-all-flashed input-rows)
  )
