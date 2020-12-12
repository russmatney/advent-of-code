(ns nine.core
  (:require
   [util :refer [input]]
   [clojure.math.combinatorics :as combo]
   ))

(defn unique-ns-that-sum [sum n xs]
  (let [combs (combo/combinations xs n)]
    (->> combs
         (filter (fn [comb]
                   (-> (apply + comb)
                       (= sum))))
         first)))

(comment
  (peek '(1 2 3))
  (pop '(1 2 3))
  (cons 0 '(1 2 3))
  (-> '(1 2 3) rest (concat '(4)))
  (conj '(1 2 3) 0)

  (unique-ns-that-sum 40 2 [35 20 15 25 47])

  (->> (input "example.txt")
       (map read-string)
       (take 5)
       reverse
       butlast
       (cons 40)
       )

  (println "\n\nbreak\n\n"))

(defn first-invalid [{:keys [group-size xs]}]
  (loop [group   (->> xs (take group-size) reverse)
         next-xs (->> xs (drop group-size))]
    (if-let [next (first next-xs)]
      (let [ns (unique-ns-that-sum next 2 group)]
        (if (seq ns)
          (recur (->> group butlast (cons next))
                 (rest next-xs))
          next))
      :none-invalid)))

(comment
  (first-invalid {:group-size 5
                  :xs         (->> (input "example.txt")
                                   (map read-string))})
  (->> (input "input.txt")
       (map read-string)
       (filter #{507622668}))

  (first-invalid {:group-size 25
                  :xs         (->> (input "input.txt")
                                   (map read-string))})
  ;; 507622668
  )

(defn contiguous-ns-that-sum [sum xs]
  (loop [ns  (take 1 xs)
         xxs (rest xs)]
    (if-let [next (some-> xxs first)]
      (let [current-sum (->> ns (apply +))]
        (cond
          (= current-sum sum)
          ns

          (> current-sum sum)
          (recur (->> ns butlast) xxs)

          (< current-sum sum)
          (recur (cons next ns) (rest xxs))))
      :no-group-found)))

(comment
  (let [res
        (contiguous-ns-that-sum 127
                                (->> (input "example.txt")
                                     (map read-string))
                                )]
    (+ (apply max res) (apply min res))
    )

  (let [res
        (contiguous-ns-that-sum 507622668
                                (->> (input "input.txt")
                                     (map read-string))
                                )]
    (+ (apply max res) (apply min res))
    )
  ;; 76688505
  )
