(ns fifteen.core
  (:require [clojure.string :as string]))

(defn parse-input [s]
  (->>
    (string/split s #",")
    (map read-string)))

(def example (-> "0,3,6" parse-input))
(def input (-> "1,17,0,10,18,11,6" parse-input))

(comment
  example
  input
  )

(defn starter-map [nums]
  (->>
    ;; last starter is not added here!!
    (butlast nums)
    (map-indexed
      (fn [i n]
        [n (+ 1 i)]))
    (into {})))

(comment
  (starter-map example)
  (get (starter-map example) 0)
  )

(defn play [nums target]
  (println "new play")
  (reduce (fn [{:keys [mem last]} turn]
            (let [updated-mem (assoc mem last (- turn 1))
                  next
                  (if-let [prev-turn (get mem last)]
                    (- turn prev-turn 1)
                    0)]
              {:mem  updated-mem
               :last next}
              ))
          {:mem  (starter-map nums)
           :last (last nums)}
          (range (+ (count nums) 1) (+ target 1))))

(comment
  (play example 5)
  (play example 10)
  (:last (play example 2020))
  (:last (play input 2020)))
