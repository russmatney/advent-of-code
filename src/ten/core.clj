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


;; part 2
;; count valid adapter arrangements

(defn as-diffs [f]
  (->> (input f)
       (map read-string)
       sort
       (reduce
         (fn [{:keys [diffs last]} next]
           {:diffs (conj diffs (- next last))
            :last  next})
         {:diffs []
          :last  0})))

(comment
  (as-diffs "example.txt")
  (as-diffs "example_two.txt")
  (as-diffs "input.txt")


  (->> (as-diffs "example.txt")
       :diffs
       (partition-by #{3})
       (remove (comp #{3} first))
       (map (fn [group]
              (cond
                (= (count group) 4) 7
                (= (count group) 3) 4
                (= (count group) 2) 2
                (= (count group) 1) 1)))
       (apply *)))

(defn count-arrangements [f]
  (->> (as-diffs f)
       :diffs
       (partition-by #{3})
       (remove (comp #{3} first))
       (map (fn [group]
              ;; here we map the diff groups to their permutation count
              (cond
                (= (count group) 4) 7
                (= (count group) 3) 4
                (= (count group) 2) 2
                (= (count group) 1) 1)))
       (apply *)))

(comment
  (count-arrangements "example.txt")
  (count-arrangements "example_two.txt")
  (count-arrangements "input.txt")
  442136281481216
  )
