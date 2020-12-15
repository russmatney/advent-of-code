(ns thirteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))


(comment
  (input "schedule.txt")
  (input "example.txt")
  (input "input.txt")
  )

(defn schedule
  "Because sometimes you write a function no one asked for"
  []
  (let [sch    (input "schedule.txt")
        header (->> sch
                    first
                    ((fn [header]
                       (->
                         header
                         (string/split #"  ")
                         (->> (map string/trim))))))
        data
        (->>
          sch
          rest
          (map (fn [line]
                 (let [[_ time buses] (-> (re-seq #"(\d+)\s+(.+)" line) first)
                       buses          (string/replace buses " " "")
                       bus-labels     (drop 1 header)
                       departure-map  (->> buses
                                           (map-indexed
                                             (fn [i raw]
                                               [(nth bus-labels i) (= raw \D)]))
                                           (into {}))
                       ]
                   [time departure-map])))
          (into {}))]
    data))

(comment
  (schedule))


(defn parse-notes [f]
  (let [parsed       (input f)
        arrival-time (->> parsed first read-string)
        buses        (-> parsed second
                         (string/split #",")
                         (->> (remove #{"x"})
                              (map read-string)
                              sort))]
    {:time  arrival-time
     :buses buses}))

(comment
  (parse-notes "example.txt")
  (parse-notes "input.txt")
  )

(defn wait-time-from [time bus-id]
  [bus-id (- bus-id (mod time bus-id))])

(comment
  (wait-time-from 939 59)
  (wait-time-from 62 59)

  (let [{:keys [time buses]} (parse-notes "example.txt")]
    (->> buses
         (map (partial wait-time-from time))
         (sort-by second)
         (first)
         (apply *)
         ))

  (let [{:keys [time buses]} (parse-notes "input.txt")]
    (->> buses
         (map (partial wait-time-from time))
         (sort-by second)
         (first)
         (apply *)
         ))
  )
