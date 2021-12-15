(ns _2021.fourteen.core
  (:require [util :refer [input]]))

(defn parse-rule [rule]
  (->> rule
       (re-seq #"([A-Z])([A-Z]) -> ([A-Z])")
       first rest
       ((fn [[start end insert]]
          [(str start end) insert]))))

(comment
  (parse-rule "CH -> B"))

(defn parse [f]
  (->> f
       input
       util/partition-by-newlines
       ((fn [[template rules]]
          {:template (first template)
           :rules    (->> rules (map parse-rule) (into {}))}))))

(def ex-data (parse "example.txt"))
(def input-data (parse "input.txt"))

(defn apply-rules [rules polymer]
  (loop [[next & rst] polymer
         new-polymer  ""
         last         nil]
    (cond
      (not next)
      new-polymer

      (not last)
      (recur rst next next)

      :else
      (recur rst
             (str new-polymer (rules (str last next)) next)
             next))))

(defn do-steps [n {:keys [template rules]}]
  (->>
    (iterate (partial apply-rules rules) template)
    (take (inc n))
    last))

(defn counts [n data]
  (->>
    (do-steps n data)
    seq
    (group-by identity)
    (map (comp count second))
    )
  )

(comment
  (do-steps 1 ex-data)
  (do-steps 2 ex-data)
  (do-steps 3 ex-data)

  ;; part 1
  (->>
    (counts 10 input-data)
    sort
    ((fn [xs]
       (- (last xs) (first xs)))))
  )
