(ns _2020.eleven.core
  (:require [util :refer [input]]))

(comment
  (input "input.txt")
  (input "example.txt")
  )

(defn parse-seats [f]
  (->> (input f)
       (map (fn [row]
              (->> row
                   (map (fn [c]
                          (case c
                            \L :empty
                            \# :occupied
                            \. nil)))
                   (into []))))
       (into [])))

(comment
  (parse-seats "example.txt")
  )

(defn get-seat [seats {:keys [row col]}]
  (some-> seats
          (nth row nil)
          (nth col nil)))

(defn adjacent-seats [seats {:keys [row col]}]
  (let [prev-row  (- row 1)
        next-row  (+ row 1)
        prev-col  (- col 1)
        next-col  (+ col 1)
        adj-seats (->> [[prev-row prev-col] [prev-row col] [prev-row next-col]
                        [row prev-col] [row next-col]
                        [next-row prev-col] [next-row col] [next-row next-col]
                        ]
                       (map (fn [[r c]] {:row r :col c})))]
    (->> adj-seats
         (map (partial get-seat seats)))))

(comment
  (let [seats (parse-seats "example.txt")]
    (adjacent-seats seats {:row 2 :col 2})
    )
  )

(defn update-seat-part-1 [seats {:keys [seat row col]}]
  (let [occupied-seats
        (->>
          (adjacent-seats seats {:row row :col col})
          (filter #{:occupied})
          count)]
    (cond
      (and (= :empty seat) (= 0 occupied-seats))     :occupied
      (and (= :occupied seat) (>= occupied-seats 4)) :empty
      :else                                          seat)))

(defn apply-round [seats update-seat]
  (let [did-update? (atom nil)]
    {:did-update? did-update?
     :seats
     (doall
       (->>
         seats
         (map-indexed
           (fn [row-i row]
             (doall
               (->>
                 row
                 (map-indexed
                   (fn [col-i seat]
                     (let [updated-seat
                           (update-seat
                             seats {:row row-i :col col-i :seat seat})]
                       (when (not (= seat updated-seat))
                         (reset! did-update? true))
                       updated-seat)))))))))}))

(defn apply-until-stable [seats update-seat]
  (loop [seats seats
         round 0]
    (println "round" round)
    (if (> round 200)
      seats
      (let [{:keys [did-update? seats]} (apply-round seats update-seat)]
        (if @did-update?
          (recur seats (inc round))
          seats)))))


(comment
  (let [seats  (parse-seats "example.txt")
        stable (apply-until-stable seats update-seat-part-1)]
    (->> stable
         flatten
         (filter #{:occupied})
         count))

  (let [seats  (parse-seats "input.txt")
        stable (apply-until-stable seats update-seat-part-1)]
    (->> stable
         flatten
         (filter #{:occupied})
         count))
  )

(defn first-in-direction [seats seat direction]
  (loop [current seat]
    (let [next       {:row (+ (:row current) (:row direction))
                      :col (+ (:col current) (:col direction))}
          found-seat (get-seat seats next)]
      (cond
        found-seat                            found-seat
        (or (< (:row next) 0)
            (> (:row next) (-> seats first count))
            (< (:col next) 0)
            (> (:col next) (-> seats count))) nil
        :else                                 (recur next)))))

(defn visible-seats [seats seat]
  ;; get first seat in each direction
  (->> [[-1 -1] [-1 0] [-1 1]
        [0 -1] [0 1]
        [1 -1] [1 0] [1 1]]
       (map (fn [[r c]] {:row r :col c}))
       (map (partial first-in-direction seats seat))))

(comment
  (let [seats (parse-seats "example.txt")]
    (visible-seats seats {:row 1 :col 1})
    )
  )

(defn update-seat-part-2 [seats {:keys [seat row col]}]
  ;; looking in all directions!
  (let [occupied-seats
        (->>
          (visible-seats seats {:row row :col col})
          (filter #{:occupied})
          count)]
    (cond
      (and (= :empty seat) (= 0 occupied-seats))     :occupied
      (and (= :occupied seat) (>= occupied-seats 5)) :empty
      :else                                          seat)))

(comment
  (println "hi")
  (let [seats  (parse-seats "example.txt")
        stable (apply-until-stable seats update-seat-part-2)]
    (->> stable
         flatten
         (filter #{:occupied})
         count))

  (let [seats  (parse-seats "input.txt")
        stable (apply-until-stable seats update-seat-part-2)]
    (->> stable
         flatten
         (filter #{:occupied})
         count))
  ;; 1974
  )
