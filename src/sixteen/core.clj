(ns sixteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))


(defn parse-rules [rules]
  (->> rules
       (map (fn [line]
              (let [res               (re-seq #"([\w| ]+): (\d+)-(\d+) or (\d+)-(\d+)"
                                              line)
                    [_ label a b x y] (first res)]
                {:label  label
                 :ranges [[a b] [x y]]})
              )))
  )

(defn parse [f]
  (->> (input f)
       (partition-by #{""})
       (remove (comp #{""} first))
       ((fn [[rules your nearby]]
          {:rules          (parse-rules rules)
           :your-ticket    (-> your second (string/split #","))
           :nearby-tickets (->> nearby rest (map #(string/split % #",")))}
          ))
       ))

(comment
  (parse "example.txt")
  (parse "input.txt")
  )



(defn error-rate [f]
  (let [parsed (parse f)
        rules  (:rules parsed)
        ranges (->> rules (mapcat :ranges))]
    ranges))

(comment
  (error-rate "example.txt")
  )
