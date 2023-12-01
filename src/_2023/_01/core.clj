(ns _2023._01.core
  (:require [util :as util]
            [clojure.string :as string]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_01/" fname)))

(defn line->numbers [line]
  (let [matches (re-seq #"\d" line)
        f       (-> matches first)
        l       (-> matches last)]
    (-> (str f l)
        read-string)))

(comment
  (input "example.txt")
  (input "input.txt")

  (re-seq #"\d" "a1b2c3d4e5f")

  (->>
    (input "input.txt")
    (map line->numbers)
    (reduce +)))

(defn str->num [s]
  (case s
    "one"   1
    "two"   2
    "three" 3
    "four"  4
    "five"  5
    "six"   6
    "seven" 7
    "eight" 8
    "nine"  9
    s))

(def crazy-reg #"(\d|one|two|three|four|five|six|seven|eight|nine)")

(defn line->matches [line]
  (->>
    (reduce
      (fn [{:as agg :keys [used last]} char]
        (let [rest  (string/replace-first line used "")
              match (some->> (re-seq crazy-reg rest)
                             (map first)
                             first)]

          (cond-> agg
            (and match
                 ;; not required, but dedupes
                 (not (= match last)))
            (->
              (update :nums concat [match])
              (assoc :last match))

            true
            (update :used str char))))
      {:used "" :nums [] :last nil}
      (seq line))
    :nums))

(defn line->numbers-2 [line]
  (let [matches (line->matches line)
        f       (-> matches first str->num)
        l       (-> matches last str->num)]
    (-> (str f l)
        (read-string))))

(comment
  (input "example2.txt")
  (input "input.txt")

  (->> (re-seq crazy-reg
               #_"two1nine"
               "ddgjgcrssevensix37twooneightgt")
       (map first))

  (line->numbers-2 "two1nine")
  (line->numbers-2 "eighttwothree")
  (line->numbers-2 "685")
  (line->numbers-2 "3eight2twojqsvbtftp")
  (line->numbers-2 "ddgjgcrssevensix37twooneightgt")

  (->> (input "input.txt")
       (map line->numbers-2)
       (reduce +)))
