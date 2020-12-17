(ns sixteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(defn parse-rules [rules]
  (->> rules
       (map (fn [line]
              (let [res                    (re-seq #"([\w| ]+): (\d+)-(\d+) or (\d+)-(\d+)"
                                                   line)
                    [_ label & range-vals] (first res)
                    [a b x y]              (->> range-vals (map read-string))
                    ]
                {:label  label
                 :ranges [[a b] [x y]]})))))

(defn parse [f]
  (->> (input f)
       (partition-by #{""})
       (remove (comp #{""} first))
       ((fn [[rules your nearby]]
          {:rules          (parse-rules rules)
           :your-ticket    (-> your second
                               (string/split #",")
                               (->> (map read-string)))
           :nearby-tickets (->> nearby
                                rest
                                (map (fn [line]
                                       (->> (string/split line #",")
                                            (map read-string)))))}))))

(comment
  (parse "example.txt")
  (parse "input.txt"))

(defn val-in-any-range? [ranges v]
  (->> ranges
       (filter
         (fn [[a b]]
           (<= a v b)))
       first))

(defn error-rate [f]
  (let [parsed  (parse f)
        rules   (:rules parsed)
        ranges  (->> rules (mapcat :ranges))
        tickets (:nearby-tickets parsed)
        values  (->> tickets (mapcat (fn [x] x)))]
    (->> values
         (remove (partial val-in-any-range? ranges))
         (apply +))))

(comment
  (error-rate "example.txt")
  (error-rate "input.txt")

  )
