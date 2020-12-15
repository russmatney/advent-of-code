(ns eleven.core
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


(defn apply-round [seats]
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
                     (let [occupied-seats
                           (->>
                             (adjacent-seats
                               seats
                               {:row row-i :col col-i})
                             (filter #{:occupied})
                             count)]

                       (cond
                         (and (= :empty seat) (= 0 occupied-seats))
                         (do
                           (reset! did-update? true)
                           :occupied)
                         (and (= :occupied seat) (>= occupied-seats 4))
                         (do
                           (reset! did-update? true)
                           :empty)
                         :else seat))))))))))}))

(defn apply-until-stable [seats]
  (loop [seats seats
         round 0]
    (let [{:keys [did-update? seats]} (apply-round seats)]
      (if @did-update?
        (recur seats (inc round))
        seats))))


(comment
  (let [seats  (parse-seats "example.txt")
        stable (apply-until-stable seats)]
    (->> stable
         flatten
         (filter #{:occupied})
         count))

  (let [seats  (parse-seats "input.txt")
        stable (apply-until-stable seats)]
    (->> stable
         flatten
         (filter #{:occupied})
         count))
  )
