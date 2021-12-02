(ns _2021.two.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(comment
  (input "example.txt")
  (input "input.txt"))

(comment
  ;; part 1
  (->> (input "input.txt")
       (map #(string/split % #" "))
       (reduce (fn [acc [cmd val]]
                 (cond
                   (= cmd "forward")
                   (update acc :x + (read-string val))

                   (= cmd "up")
                   (update acc :y - (read-string val))

                   (= cmd "down")
                   (update acc :y + (read-string val))))
               {:x 0 :y 0})
       ((fn [{:keys [x y]}] (* x y)))))

(comment
  ;; part 2
  (->> (input "input.txt")
       (map #(string/split % #" "))
       (reduce (fn [acc [cmd val]]
                 (let [v   (read-string val)
                       aim (:aim acc)
                       ]
                   (cond
                     (= cmd "forward")
                     (-> acc
                         (update :x + v)
                         (update :y + (* aim v)))

                     (= cmd "up")
                     (update acc :aim - v)

                     (= cmd "down")
                     (update acc :aim + v))))
               {:x 0 :y 0 :aim 0})
       ((fn [{:keys [x y]}] (* x y)))))
