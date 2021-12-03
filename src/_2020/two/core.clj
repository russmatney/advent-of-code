(ns _2020.two.core
  "Validate passwords according to arbitrary policies.

  Part 1 Examples:
  - '1-3 a: abc' (valid)
  - '10-12 a: aaaaabaaaac' (invalid, only 9 a's)

  Part 2 Examples:
  - '1-3 a: abc' (valid, pos 1 is a, pos 3 is not)
  - '10-12 a: aaaaabaaaacd' (valid, pos 10 is a, 12 is not)
  - '2-4 a: baba' (invalid, both pos 2 and 4 are a, 12 is not)

  How many passwords are valid?
  "

  (:require [util :refer [input]]))

(defn parse [s]
  (let [parsed             (re-seq #"(\d+)-(\d+) ([a-z]): ([a-z]+)" s)
        [min max char pwd] (-> parsed first rest)]
    {:min  (read-string min)
     :max  (read-string max)
     :char (first char)
     :pwd  pwd})
  )

(defn is-valid? [{:keys [min max char pwd]}]
  (let [chars (->> pwd
                   (filter #{char})
                   (apply str))
        num   (count chars)]
    (and (>= num min)
         (<= num max))))

(def --sample-input
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

(comment
  (input "input.txt")

  (re-seq #"(\d+)-(\d+) ([a-z]): ([a-z]+)" "1-3 a: abc")

  (filter (fn [c]
            (= c \b)) "abc")
  (= (first "x") \x)
  (first "x")

  (->> --sample-input
       (map parse)
       (map is-valid?))


  (->> (input "input.txt")
       (map parse)
       (map is-valid?)
       (filter #{true})
       count)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-2 [s]
  (let [parsed               (re-seq #"(\d+)-(\d+) ([a-z]): ([a-z]+)" s)
        [pos1 pos2 char pwd] (-> parsed first rest)]
    {:pos1 (read-string pos1)
     :pos2 (read-string pos2)
     :char (first char)
     :pwd  pwd})
  )

(defn is-valid-2? [{:keys [pos1 pos2 char pwd]}]
  (let [pos1? (= (nth pwd (dec pos1)) char)
        pos2? (= (nth pwd (dec pos2)) char)]
    (and (or pos1? pos2?)
         (not (and pos1? pos2?)))))

(comment
  (println "break")

  (nth "abc" 2)

  (->> --sample-input
       (map parse-2)
       (map is-valid-2?)
       )

  (->> (input "input.txt")
       (map parse-2)
       (map is-valid-2?)
       (filter #{true})
       count)
  )
