(ns _2023._03.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_03/" fname)))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn digit? [x]
  (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} x))

(defn ->grid
  ([raw] (->grid nil raw))
  ([pred raw]
   (->> raw
        (map-indexed
          (fn [y row]
            (->> row (map-indexed
                       (fn [x char]
                         (if pred
                           (when (pred char)
                             [[x y] char])
                           [[x y] char])))
                 (filter seq)
                 (into []))))
        (filter seq)
        (into [])
        (apply concat)
        (into {}))))

(defn part? [char]
  (not (or (= \. char) (digit? char))))

(defn ->part-locs [raw]
  (->grid part? raw))

(comment
  (->part-locs (input "example.txt"))
  (->grid (input "example.txt"))

  (->> "45..*.4"
       (map (fn [char]
              (digit? char)
              ))))

(defn neighbors [[x y]]
  #{[x (inc y)]
    [x (dec y)]
    [(inc x) y]
    [(dec x) y]
    [(inc x) (inc y)]
    [(dec x) (dec y)]
    [(dec x) (inc y)]
    [(inc x) (dec y)]})

(defn x-num-neighbors [grid [x y]]
  (let [->x-nbrs
        (fn [->x]
          (loop [new-coord [(->x x) y] nbrs []]
            (cond
              (nil? (grid new-coord)) nbrs
              (digit? (grid new-coord))
              (recur [(-> (first new-coord) ->x) y] (concat nbrs [new-coord]))
              :else                   nbrs)))
        left-nbrs  (->x-nbrs dec)
        right-nbrs (->x-nbrs inc)]
    (concat (reverse left-nbrs) [[x y]] right-nbrs)))

(comment
  (x-num-neighbors (->grid (input "example.txt")) [3 2])
  (x-num-neighbors (->grid (input "example.txt")) [2 2])
  (x-num-neighbors (->grid (input "example.txt")) [0 0]))

(defn coord->number [grid coord]
  (let [num-coords (x-num-neighbors grid coord)]
    [(first num-coords)
     (->> num-coords
          (map grid)
          (apply str)
          (parse-long))]))

(comment
  (coord->number (->grid (input "example.txt")) [2 2])
  (coord->number (->grid (input "example.txt")) [0 0]))

(defn part-numbers
  ([raw] (part-numbers nil raw))
  ([opts raw]
   (let [grid      (->grid raw)
         part-locs (->part-locs raw)]
     ;; symbols->neighbor-digits
     (cond->> part-locs
       true
       (map (fn [[coord sym]]
              [sym
               (let [nbrs (neighbors coord)]
                 (->> nbrs
                      (filter (fn [nbr]
                                (let [char (grid nbr)]
                                  (digit? char))))
                      (map (fn [nbr] [nbr (grid nbr)]))))]))

       (:symbol opts) (filter (fn [[sym _nbrs]]
                                (#{(:symbol opts)} sym)))

       true
       (map (fn [[_sym nbrs]]
              (->> nbrs
                   (map (fn [[coord _digit]]
                          (coord->number grid coord)))
                   (into {}))))

       (:part-count opts)
       (filter (fn [xs]
                 (= (:part-count opts) (count xs))))))))

(comment
  (->>
    (part-numbers (input "example.txt"))
    (map vals)
    (apply concat)
    (reduce +)
    )

  ;; part 1
  (->>
    (part-numbers (input "input.txt"))
    (map vals)
    (apply concat)
    (reduce +))

  ;; part 2
  (->>
    (input "example.txt")
    (part-numbers
      {:symbol     \*
       :part-count 2})
    (map vals)
    (map #(apply * %))
    (reduce +))

  (->>
    (input "input.txt")
    (part-numbers
      {:symbol     \*
       :part-count 2})
    (map vals)
    (map #(apply * %))
    (reduce +)))
