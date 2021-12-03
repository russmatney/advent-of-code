(ns _2020.six.core
  (:require [util :refer [input]]
            [clojure.string :as string]
            [clojure.set :as set]))

(comment

  ;; part 1
  (->>
    (input "input.txt")
    (partition-by #{""})
    (remove (comp #{""} first))
    (map string/join)
    (map set)
    (map count)
    (apply +))


  ;; part 2
  (->>
    (input "input.txt")
    (partition-by #{""})
    (remove (comp #{""} first))
    (map (fn [answers]
           (->> answers
                (map set)
                (apply set/intersection))))
    (map count)
    (apply +)
    )

  )
