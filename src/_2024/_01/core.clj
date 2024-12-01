(ns _2024._01.core
  (:require
   [clojure.string :as string]
   [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2024/_01/" fname)))

;; part 1
(comment
  (input "input.txt")
  (input "example.txt")

  (->>
    (input "input.txt")
    (map #(-> % (string/split #" ")
              (->> (remove (comp #{0} count)))))
    (reduce (fn [acc next]
              (-> acc
                  (update 0 conj (read-string (first next)))
                  (update 1 conj (read-string (last next)))))
            [[] []])
    (map sort)
    (map #(into [] %))
    ((fn [[f s]]
       (loop [ix       0
              distance 0]
         (println "ix" ix "distance" distance)
         (if (>= ix (count f))
           distance
           (recur (inc ix)
                  (+ distance
                     (abs
                       (-
                         (get f ix)
                         (get s ix)))))))))))
