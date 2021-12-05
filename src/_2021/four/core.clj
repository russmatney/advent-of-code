(ns _2021.four.core
  (:require [util :refer [input]]
            [clojure.string :as string]
            [clojure.set :as set]))

(comment
  (input "example.txt")
  (input "input.txt")
  )

(defn parse-board [lines]
  (let [{:keys [parsed] :as acc}
        (reduce (fn [acc line]
                  (let [parsed-line
                        (-> line (string/split #" +")
                            (->>
                              (remove #{"" nil})
                              (map read-string)
                              vec))
                        numbers (set parsed-line)]
                    (-> acc
                        (update :winning-sets conj numbers)
                        (update :all-numbers set/union numbers)
                        (update :parsed conj parsed-line))))
                {:winning-sets #{}
                 :all-numbers  #{}
                 :parsed       []}
                lines)

        vertical-wins (->> parsed
                           (reduce (fn [acc line]
                                     (if-not acc
                                       ;; create initial sets
                                       (->> line (map (fn [n] #{n})) vec)
                                       ;; append to existing sets
                                       (->> acc
                                            (map-indexed (fn [i win]
                                                           (let [num (nth line i)]
                                                             (conj win num)))))))
                                   nil)
                           set)]
    (-> acc
        (update :winning-sets set/union vertical-wins)
        (merge {:raw lines}))))

(comment
  (parse-board '("1 2 3" "4 5   6" " 7  8    9"))
  (string/split " 7  8    9" #" +")

  (->> '(1 2 3)
       (map (fn [n] #{n}))
       vec
       )

  )

(defn parse [f]
  (->> (input f)
       (util/partition-by-newlines)
       ((fn [lines]
          {:numbers (-> lines first first (string/split #","))
           :boards  (->> lines rest (map-indexed
                                      (fn [i x]
                                        (merge (parse-board x) {:i i}))))}))))

(defn did-board-win? [board in-play]
  (let [{:keys [winning-sets]} board]
    (->> winning-sets (filter #(set/subset? % in-play)) first)))

(comment
  (parse "example.txt")

  (println "\n\n")

  (set/subset? #{24 4 21 17 14} #{9 22 4 14 21 17 25 15 7 5 18 12 13 24 6 0 11 2 16 10 23})

  ;; first board to win
  (let [{:keys [boards numbers]} (parse "input.txt")]
    (loop [remaining numbers
           in-play   #{}
           last-num  nil]
      (if
          (not (seq remaining)) nil

          (let [winning-board
                (->> boards (filter #(did-board-win? % in-play)) first)
                ]
            (if winning-board
              (let [unmarked-numbers (set/difference (:all-numbers winning-board) in-play)]
                {:winning-board    winning-board
                 :in-play          in-play
                 :last-num         last-num
                 :unmarked-numbers unmarked-numbers
                 :score            (* last-num (->> unmarked-numbers (reduce +)))})

              (let [num (-> remaining first read-string)]
                (recur (rest remaining) (conj in-play num) num)))))))
  )

;; part2

(comment

  ;; last board to win
  (let [{:keys [boards numbers]} (parse "example.txt")]
    (loop [remaining numbers
           in-play   #{}
           last-num  nil
           boards    boards]
      (if
          (not (seq remaining)) nil

          (let [remaining-boards (->> boards (remove #(did-board-win? % in-play)))]
            (if (and
                  ;; was only one board left to win
                  (#{1} (count boards))
                  ;; and it won!
                  (#{0} (count remaining-boards)))
              (let [last-board       (first boards)
                    unmarked-numbers (set/difference (:all-numbers last-board) in-play)]
                {:winning-board    last-board
                 :in-play          in-play
                 :last-num         last-num
                 :unmarked-numbers unmarked-numbers
                 :score            (* last-num (->> unmarked-numbers (reduce +)))})

              (let [num (-> remaining first read-string)]
                (recur (rest remaining) (conj in-play num) num remaining-boards)))))))
  )
