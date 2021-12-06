(ns _2021.six.core
  (:require [util :refer [input]])
  )

(comment
  (input "example.txt")
  (input "input.txt")
  )

(defn initial [f]
  (->> f
       input
       first
       (re-seq #"\d" )
       (map read-string)))

(defn process-day [fish]
  (let [new-fish-ct  (->> fish (filter zero?) count)
        updated-fish (->> fish (map dec) (map (fn [n] (if (< n 0) 6 n))))]
    (apply conj updated-fish (repeat new-fish-ct 8))))

(comment
  (def day-goal 256)
  (loop [fish (initial "example.txt")
         ;; ct   (count fish)
         day  0]
    (if (#{day-goal} day)
      (count fish)
      (recur
        (process-day fish)
        (inc day)))))

;; part 2

(defn fish-timers [f]
  (->> (initial f)
       (group-by identity)
       ((fn [m] (zipmap (keys m) (->> m vals (map count)))))))

(defn update-timers [fish]
  (let [zero-ct  (get fish 0)
        one-ct   (get fish 1)
        two-ct   (get fish 2)
        three-ct (get fish 3)
        four-ct  (get fish 4)
        five-ct  (get fish 5)
        six-ct   (get fish 6)
        seven-ct (get fish 7)
        eight-ct (get fish 8)]
    {0 one-ct
     1 two-ct
     2 three-ct
     3 four-ct
     4 five-ct
     5 six-ct
     6 (+ (or seven-ct 0) (or zero-ct 0))
     7 eight-ct
     8 zero-ct}))

(comment
  (update-timers
    (update-timers
      (fish-timers "example.txt")))

  ;; 3 4 3 1 2
  ;; 2 3 2 0 1
  ;; 1 2 1 6 0 8
  ;; 0 1 0 5 6 7 8
  ;; 6 0 6 4 5 6 7 8 8
  ;; 5 6 5 3 4 5 6 7 7 8

  ;; map to {num: count}
  (->> (initial "example.txt")
       (group-by identity)
       ((fn [m] (zipmap (keys m)
                        (->> m vals (map count))))))

  (fish-timers "example.txt")
  (fish-timers "input.txt")

  (loop [fish           (fish-timers "input.txt")
         days-remaining 256]
    (if (zero? days-remaining)
      (->> fish vals (apply +))
      (recur
        (update-timers fish)
        (dec days-remaining))))
  )
