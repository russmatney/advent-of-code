(ns _2022._04.core
  (:require
   [util :as util]
   [clojure.string :as string]))

(defn input [f]
  (->>
    (util/parse-input (str "src/_2022/_04/" f) {:split ","})
    (map (fn [pairs]
           (->> pairs
                (map (fn [r]
                       (->> (string/split r #"-")
                            (map read-string)
                            (into []))))
                (into []))))))

(defn full-overlap? [[a b] [x y]]
  (or
    (and (<= a x) (>= b y))
    (and (<= x a) (>= y b))))

(comment
  (full-overlap? [2 4] [6 8])
  (full-overlap? [2 4] [3 3])
  (full-overlap? [3 3] [2 4])
  (full-overlap? [3 7] [2 4])
  (full-overlap? [3 7] [3 4]))

(defn partial-overlap? [[a b] [x y]]
  (or
    (<= a y b)
    (<= a x b)
    (<= x a y)
    (<= x b y)))

(comment
  (partial-overlap? [2 4] [6 8])
  (partial-overlap? [2 4] [3 3])
  (partial-overlap? [3 3] [2 4])
  (partial-overlap? [3 7] [2 4])
  (partial-overlap? [3 7] [3 4]))

(defn overlapping-pairs [f overlap?]
  (->>
    (input f)
    (filter #(apply overlap? %))
    count))

(comment
  (overlapping-pairs "example.txt" full-overlap?)
  (overlapping-pairs "input.txt" full-overlap?)
  (overlapping-pairs "example.txt" partial-overlap?)
  (overlapping-pairs "input.txt" partial-overlap?))
