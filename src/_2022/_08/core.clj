(ns _2022._08.core
  (:require
   [clojure.string :as string]))

(defn input [f]
  (->>
    (slurp (str "src/_2022/_08/" f))
    (string/split-lines)
    (map-indexed (fn [y line]
                   (->> line
                        (map-indexed
                          (fn [x tree]
                            [[x y] (read-string (str tree))])))))
    (mapcat identity)
    (into {})))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn is-visible? [grid n [x y]]
  (cond
    (or
      (zero? x)
      (zero? y)
      (#{(- n 1)} x)
      (#{(- n 1)} y))
    true

    :else
    false))

(defn visible-trees [f]
  (let [grid   (input f)
        n      (-> (str "src/_2022/_08/" f) slurp string/split-lines count)
        points (for [x (range n) y (range n)] [x y])]
    (->> points
         (filter
           (fn [point]
             (is-visible? grid n point))))
    )
  )

(comment
  (visible-trees "example.txt")
  )
