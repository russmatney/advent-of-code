(ns _2023._04.core
  (:require [util :as util]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_04/" fname)))

(comment
  (input "input.txt")
  (input "example.txt"))

(defn parse [data]
  (->> data
       (map (fn [card]
              (let [[id-bit numbers]     (string/split card #":")
                    id                   (->> (re-seq #"(\d+)" id-bit)
                                              (map first)
                                              first parse-long)
                    [winning-n played-n] (string/split numbers #"\|" 2)
                    winning
                    (->> (re-seq #"(\d+)" winning-n)
                         (map first)
                         (map parse-long))
                    played
                    (->> (re-seq #"(\d+)" played-n)
                         (map first)
                         (map parse-long))]
                {:id id :winning winning :played played})))))

(comment
  (parse (input "example.txt"))
  (parse (input "input.txt")))

(defn matches [game]
  (let [{:keys [winning played]}
        (-> game
            (update :winning #(into #{} %))
            (update :played #(into #{} %)))]
    (count (set/intersection winning played))))

(defn points [game]
  (reduce
    (fn [tot _i]
      (if (zero? tot) 1 (* 2 tot)))
    0 (range (matches game))))

(comment
  ;; part 1
  (->> "example.txt" input parse
       (map points)
       (reduce +))
  (->> "input.txt" input parse
       (map points)
       (reduce +)))

(defn ->by-id [parsed]
  (->> parsed
       (group-by :id)
       (map (fn [[id xs]] [id (first xs)]))
       (into {})))

(comment
  (->> "example.txt" input parse ->by-id))

(defn process-copies [games]
  (:by-id
   (reduce
     (fn [{:as _agg :keys [by-id curr-id]} game]
       (let [current-copies (:copies (by-id curr-id))
             copy-count     (matches game)]
         {:by-id
          (reduce
            (fn [by-id i]
              (if (get by-id i)
                (update-in by-id [i :copies] + current-copies)
                (update by-id :extra + current-copies)))
            by-id
            (range (inc curr-id) (+ (inc curr-id) copy-count)))
          :curr-id (inc curr-id)}))
     {:by-id   (->by-id games)
      :curr-id 1}
     games)))

(comment
  ;; part 2
  (let [proc   (->> "input.txt" input parse
                    (map #(assoc % :copies 1))
                    process-copies)
        extra  (:extra proc 0)
        copies (->> proc vals (map :copies) (remove nil?) (reduce +))]
    (+ extra copies)))
