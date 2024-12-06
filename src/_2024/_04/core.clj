(ns _2024._04.core
  (:require [util :as util]
            [grid :as grid]))

(defn input [fname]
  (util/parse-input (str "src/_2024/_04/" fname)
                    {}))

(comment
  (input "example.txt")
  (input "input.txt")

  (->
    (input "example.txt")
    (grid/parse-grid)
    (grid/val-at {:x 3 :y 4}))

  (->
    (input "example.txt")
    (grid/parse-grid)
    (grid/valid-neighbors {:x 0 :y 3})))

(defn found-searches [grid coord])

(comment
  (let [grid   (-> (input "example.txt") (grid/parse-grid))
        coords (grid/all-coords grid)
        search (->> "XMAS" (map str))]
    (->> coords
         (map #_#(found-searches grid %)
              (fn [{:keys [x y val] :as coord}]
                (if-not (#{(first search)} val)
                  0
                  (let [valid-nbrs (grid/valid-neighbors grid coord)]
                    (->> valid-nbrs
                         (filter (fn [{:keys [val]}]
                                   (#{(second search)} val))))))
                ))))
  )
