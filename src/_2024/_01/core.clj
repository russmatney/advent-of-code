(ns _2024._01.core
  (:require
   [clojure.string :as string]
   [util :as util]))

(defn input [fname]
  (->>
    (util/parse-input (str "src/_2024/_01/" fname))
    (map #(-> % (string/split #" ")
              (->> (remove (comp #{0} count)))))
    (reduce (fn [acc next]
              (-> acc
                  (update 0 conj (read-string (first next)))
                  (update 1 conj (read-string (last next)))))
            [[] []])))

(comment
  (input "input.txt")
  (input "example.txt")

  ;; part 1
  (->>
    (input "input.txt")
    (map sort)
    (map #(into [] %))
    ((fn [[left right]]
       (loop [ix       0
              distance 0]
         (if (>= ix (count left))
           distance
           (recur (inc ix)
                  (+ distance (abs (- (get left ix) (get right ix))))))))))

  ;; part 2
  (->>
    (input "input.txt")
    ((fn [[left right]]
       (let [freqs (frequencies right)]
         (->> left
              (map (fn [x] (* x (or (freqs x) 0))))
              (reduce + 0)))))))
