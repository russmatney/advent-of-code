(ns ten.core
  "
  Adapter Hell!

  Connect adapters in order from 0 to (largest adapter + 3)
  Count the 1, 2, 3 volt differences.
  Multiply the 1 volt diff count by the 3 volt diff count.
  "
  (:require [util :refer [input]]))

(comment
  (input "example.txt")
  (input "example_two.txt")
  (input "input.txt")
  )


(defn count-diffs [f]
  (let [nums (->> (input f)
                  (map read-string)
                  sort)
        m    (+ (apply max nums) 3)]
    (loop [remaining nums
           current-v 0
           totes     {1    0
                      2    0
                      3    0
                      :max m}]
      (if-not (seq remaining)
        ;; add last step as 3 higher than max
        (update totes 3 inc)
        (let [next-v (first remaining)
              diff   (- next-v current-v)
              totes  (update totes diff #(inc %))]
          (recur (rest remaining)
                 next-v
                 totes))))))

(comment
  (count-diffs "example.txt")
  (count-diffs "example_two.txt")
  (let [res (count-diffs "input.txt")]
    (* (get res 1) (get res 3)))
  )
