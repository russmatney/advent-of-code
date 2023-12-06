(ns _2023._06.core
  (:require
   [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_06/" fname)))

(comment
  (input "example.txt"))

(defn races [data]
  (let [line->ints #(->> % (re-seq #"(\d+)") (map second) (map parse-long))
        times      (-> data first line->ints)
        distances  (-> data second line->ints)]
    (->> (zipmap times distances)
         (map (fn [[t d]] {:t t :d d})))))

(comment
  (races (input "example.txt"))
  (races (input "input.txt")))

(defn one-race [data]
  (let [line->ints #(->> % (re-seq #"(\d+)") (map second) (apply str) parse-long)
        time       (-> data first line->ints)
        distance   (-> data second line->ints)]
    {:t time
     :d distance}))

(comment
  (one-race (input "example.txt"))
  (one-race (input "input.txt")))

(defn winning-edge-times [{:as _race :keys [t d]}]
  ;; x = (T +/- sqrt(T^2 - 4D))/2
  (let [n  (Math/sqrt (- (* t t) (* 4 d)))
        x1 (long (/ (- t n) 2))
        x2 (- t x1)]
    (dec (- x2 x1))))

(comment
  (->>
    (races (input "example.txt"))
    (map winning-edge-times)
    (apply *))

  (winning-edge-times (one-race (input "example.txt")))
  (winning-edge-times (one-race (input "input.txt"))))
