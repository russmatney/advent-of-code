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

(comment

  (counts-to-dec [1 0 1 1 0] #(if (> % 0) 1 0))
  (counts-to-dec [1 0 1 1 0] #(if (> % 0) 0 1))

  (Integer/parseInt "10110" 2)

  (let [f   "input.txt"
        len (->> (input f) first count)]
    (->> (input f)
         (reduce (fn [acc val]
                   (->> acc
                        (map-indexed
                          (fn [i acc-v]
                            (let [bit (nth val i)]
                              (case bit
                                \0 (dec acc-v)
                                \1 (inc acc-v)))))
                        vec))
                 (vec (repeat len 0)))
         ((fn [acc]
            (let [gamma   (counts-to-dec acc #(if (> % 0) 1 0))
                  epsilon (counts-to-dec acc #(if (> % 0) 0 1))]
              (* gamma epsilon))))))
  )
