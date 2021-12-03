(ns _2021.three.core
  (:require [util :refer [input]]))


(comment
  (input "example.txt")
  (input "input.txt")
  )

(defn counts-to-dec [acc pred]
  (->> acc
       (map pred)
       (apply str)
       (#(Integer/parseInt % 2)))
  )

(defn acc-bit-diff
  "Converts a list of binary strs into a list of diffs by bit."
  [xs]
  (let [len (->> xs first count)]
    (reduce (fn [acc val]
              (->> acc
                   (map-indexed
                     (fn [i acc-v]
                       (let [bit (nth val i)]
                         (case bit
                           \0 (dec acc-v)
                           \1 (inc acc-v)))))
                   vec))
            (vec (repeat len 0))
            xs)))

(defn most-common-bits [xs]
  (-> xs
      acc-bit-diff
      (counts-to-dec #(cond
                        (> % 0)  1
                        (#{0} %) 1
                        :else    0))))

(defn least-common-bits [xs]
  (-> xs
      acc-bit-diff
      (counts-to-dec #(cond
                        (> % 0)  0
                        (#{0} %) 0
                        :else    1))))

(comment
  (counts-to-dec [1 0 1 1 0] #(if (> % 0) 1 0))
  (counts-to-dec [1 0 1 1 0] #(if (> % 0) 0 1))

  (Integer/parseInt "10110" 2)

  (acc-bit-diff (input "input.txt"))
  (acc-bit-diff (input "example.txt"))

  ;; part 1
  (let [acc     (acc-bit-diff (input "example.txt"))
        gamma   (counts-to-dec acc #(if (> % 0) 1 0))
        epsilon (counts-to-dec acc #(if (> % 0) 0 1))]
    (* gamma epsilon))

  (let [f       "example.txt"
        gamma   (most-common-bits (input f))
        epsilon (least-common-bits (input f))]
    (* gamma epsilon)))


;; part 2

(defn diff-bit-n [xs n]
  (reduce
    (fn [acc val]
      (let [bit (nth val n)]
        (case bit
          \0 (dec acc)
          \1 (inc acc))))
    0
    xs))

(defn bit-crit [type xs n]
  (-> (diff-bit-n xs n)
      ((fn [diff]
         (case type
           :oxygen (cond
                     (> diff 0)  \1
                     (#{0} diff) \1
                     (< diff 0)  \0)
           :co2    (cond
                     (> diff 0)  \0
                     (#{0} diff) \0
                     (< diff 0)  \1))))))

(defn rating [type xs]
  (loop [remaining xs n 0]
    (if (or (< (count remaining) 2)
            (> n (count (first remaining))))
      (first remaining)
      (let [bit-criteria (bit-crit type remaining n)
            filtered
            (->> remaining
                 (filter (fn [bin]
                           (#{bit-criteria} (nth bin n)))))]
        (recur filtered (inc n))))))

(defn life-support-rating [xs]
  (let [rating-dec (fn [type xs] (-> (rating type xs) (Integer/parseInt 2)))]
    (* (rating-dec :oxygen xs) (rating-dec :co2 xs)))
  )

(comment
  (input "example.txt")

  (acc-bit-diff (input "example.txt"))
  (diff-bit-n (input "example.txt") 1)
  (bit-crit :oxygen (input "example.txt") 0)
  (bit-crit :oxygen (input "example.txt") 1)
  (bit-crit :oxygen (input "example.txt") 2)
  (bit-crit :oxygen (input "example.txt") 3)
  (bit-crit :oxygen (input "example.txt") 4)
  (bit-crit :co2 (input "example.txt") 0)
  (bit-crit :co2 (input "example.txt") 1)
  (bit-crit :co2 (input "example.txt") 2)
  (bit-crit :co2 (input "example.txt") 3)
  (bit-crit :co2 (input "example.txt") 4)

  (nth "00101" 2)

  (rating :oxygen (input "example.txt"))
  (rating :co2 (input "example.txt"))

  (rating :oxygen (input "input.txt"))
  (rating :co2 (input "input.txt"))

  (life-support-rating (input "example.txt"))
  (life-support-rating (input "input.txt"))

  )
