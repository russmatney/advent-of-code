(ns _2022._05.core
  (:require [util :as util]
            [clojure.string :as string]))

(defn input [f]
  (util/parse-input (str "src/_2022/_05/" f) {:partition? true}))

(comment
  (input "input.txt")
  (input "example.txt"))

(defn crates [data]
  (let [raw            (-> data first reverse)
        stack-nums-str (-> raw first)
        stacks         (->> stack-nums-str
                            (#(re-seq #"\d" %))
                            (map (fn [n] [n []]))
                            (into {}))
        crate-strs     (rest raw)]
    (->> stacks
         (map
           (fn [[num _stk]]
             (let [idx        (string/index-of stack-nums-str num)
                   stk-crates (->> crate-strs
                                   (map #(get % idx))
                                   (remove nil?)
                                   (remove #{\space})
                                   (into []))]
               [num stk-crates])))
         (into {}))))

(comment
  (let [x [\Z \N]] (pop (conj x \P)))
  (-> (input "example.txt") crates))

(defn moves [data]
  (->>
    (second data)
    (map (fn [move]
           (let [count (-> (re-seq #"move (\d+)" move) first second)
                 from  (-> (re-seq #"from (\d)" move) first second)
                 to    (-> (re-seq #"to (\d)" move) first second)]
             {:count (read-string count)
              :from  from
              :to    to})))))

(comment
  (-> (re-seq #"move (\d+) from (\d) to (\d)" "move 1 from 2 to 1") first second)
  (re-seq #"move (\d+) from (\d) to (\d)" "move 1 from 2 to 1")
  ;; (["move 1 from 2 to 1" "1" "2" "1"])
  (-> (input "example.txt") moves))

(defn process-move
  "Moves a single crate from `from` to `to`."
  [crates {:keys [from to]}]
  (let [moving-crate (peek (crates from))
        new-from     (pop (crates from))
        new-to       (conj (crates to) moving-crate)]
    (-> crates
        (assoc from new-from)
        (assoc to new-to))))

(comment
  (peek [])
  (pop [\N])
  (process-move {"1" [\N \Z] "2" [\A \B] "3" []}
                {:from "3" :to "2"}))

(defn process-move-9001 [crates {:keys [from to count]}]
  (let [moving-crates (->> (crates from) reverse (take count) reverse (into []))
        new-from      (->> (crates from) reverse (drop count) reverse (into []))
        new-to        (apply conj (crates to) moving-crates)]
    (-> crates
        (assoc from new-from)
        (assoc to new-to))))

(comment
  (->> [\N \Z \B] reverse (take 2) reverse (into []))
  (->> [\N \Z \B] reverse (drop 2) reverse (into []))
  (->> [\A \X] (#(apply conj % [\Z \B])))
  (pop [\N])
  (process-move-9001 {"1" [\M \N \Z] "2" [\A \B] "3" []}
                     {:from "1" :to "2" :count 2}))

(defn process-moves [crates moves crate-model]
  (reduce
    (fn [crates move]
      (cond
        (#{9000} crate-model)
        (->> (iterate (fn [crates] (process-move crates move)) crates)
             (take (inc (:count move)))
             last)

        (#{9001} crate-model)
        (process-move-9001 crates move)))
    crates
    moves))

(defn top-of-stacks [crates]
  (->> crates
       (sort-by first) ;; be sure to sort by stack num
       (map (comp peek second))
       (apply str)))

(defn rearrange
  ([data] (rearrange data 9000))
  ([data model]
   (process-moves (crates data) (moves data) model)))

(comment
  (-> (input "example.txt") rearrange top-of-stacks)
  (-> (input "example.txt") (rearrange 9001) top-of-stacks)
  (-> (input "input.txt") rearrange)
  (-> (input "input.txt") rearrange top-of-stacks)
  (-> (input "input.txt") (rearrange 9001) top-of-stacks))
