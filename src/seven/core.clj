(ns seven.core
  (:require [util :refer [input]]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-rule [line]
  (->> (string/split line #"bags contain")
       (map string/trim)
       ((fn [[k bags-str]]
          [k (->> (re-seq #"(\d) ([\w\s]+) bag" bags-str)
                  (map #(drop 1 %))
                  (map (fn [[n bag-str]]
                         {:n     (read-string n)
                          :color (string/replace bag-str " bags." "")})))]))))

(defn build-rules [f]
  (->>
    (input f)
    (map parse-rule)
    (into {}))
  )

(comment
  (build-rules "example.txt")
  (build-rules "input.txt")
  )

(defn direct-parents [rules color]
  (->> rules
       (filter (fn [[_k bags]]
                 (->> bags
                      (filter (comp #{color} :color))
                      seq)))
       (map first)
       (into #{})))

(defn collect-ancestors [rules colors]
  (loop [parents #{}
         bags    colors]
    (let [new-parents   (->> bags (map #(direct-parents rules %)) (apply set/union))
          all-ps        (set/union parents new-parents)
          unique-new-ps (set/difference new-parents parents)]
      (if (seq unique-new-ps)
        (recur all-ps unique-new-ps)
        all-ps)))
  )

(comment
  (collect-ancestors (build-rules "example.txt") #{"shiny gold"})
  (direct-parents (build-rules "example.txt") "shiny gold")

  (count (collect-ancestors (build-rules "input.txt") #{"shiny gold"}))
  )


(defn bag-count-for [rules {:keys [n color]}]
  (let [child-bags  (rules color)
        child-count (if (seq child-bags)
                      (let [x (->> child-bags
                                   (map (partial bag-count-for rules))
                                   (apply +))]
                        x)
                      0)
        total-count (+ n (* n child-count))
        ]
    total-count
    ))

(comment
  ;; and subtract one to remove the initial bag from the count
  (bag-count-for (build-rules "example-two.txt")
                 {:n 1 :color "dark blue"})
  (bag-count-for (build-rules "example-two.txt")
                 {:n 1 :color "shiny gold"})
  (bag-count-for (build-rules "example.txt")
                 {:n 1 :color "shiny gold"})
  (bag-count-for (build-rules "input.txt")
                 {:n 1 :color "shiny gold"})
  )
