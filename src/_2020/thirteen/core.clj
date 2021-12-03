(ns _2020.thirteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))


(comment
  (input "schedule.txt")
  (input "example.txt")
  (input "input.txt")
  )

(defn schedule
  "Because sometimes you write a function no one asked for"
  []
  (let [sch    (input "schedule.txt")
        header (->> sch
                    first
                    ((fn [header]
                       (->
                         header
                         (string/split #"  ")
                         (->> (map string/trim))))))
        data
        (->>
          sch
          rest
          (map (fn [line]
                 (let [[_ time buses] (-> (re-seq #"(\d+)\s+(.+)" line) first)
                       buses          (string/replace buses " " "")
                       bus-labels     (drop 1 header)
                       departure-map  (->> buses
                                           (map-indexed
                                             (fn [i raw]
                                               [(nth bus-labels i) (= raw \D)]))
                                           (into {}))
                       ]
                   [time departure-map])))
          (into {}))]
    data))

(comment
  (schedule))


(defn parse-notes [f]
  (let [parsed       (input f)
        arrival-time (->> parsed first read-string)
        buses        (-> parsed second
                         (string/split #",")
                         (->> (remove #{"x"})
                              (map read-string)
                              sort))]
    {:time  arrival-time
     :buses buses}))

(comment
  (parse-notes "example.txt")
  (parse-notes "input.txt")
  )

(defn wait-time-from [time bus-id]
  [bus-id (- bus-id (mod time bus-id))])

(comment
  (wait-time-from 939 59)
  (wait-time-from 62 59)

  (let [{:keys [time buses]} (parse-notes "example.txt")]
    (->> buses
         (map (partial wait-time-from time))
         (sort-by second)
         (first)
         (apply *)
         ))

  (let [{:keys [time buses]} (parse-notes "input.txt")]
    (->> buses
         (map (partial wait-time-from time))
         (sort-by second)
         (first)
         (apply *)
         ))
  )

;; part 2


;; cheated for helpers on this one.... :(
;; https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd [a b]
  (if (zero? b) a
      (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& ns] (reduce lcm (remove nil? ns)))

(comment
  (lcmv 2 3 4 nil))

(defn parse-buses [f]
  (-> (input f)
      second
      (string/split #",")
      (->> (map (fn [val]
                  (if (#{"x"} val) nil
                      (read-string val))))
           (into []))))

(defn buses-with-offsets [f]
  (let [buses (parse-buses f)]
    (->> buses
         (map-indexed
           (fn [i bus]
             (if (nil? bus)
               nil
               {:bus    bus
                :offset i})))
         (remove nil?))))

(comment
  (buses-with-offsets "example.txt")
  )

(defn valid-bus?
  [{:keys [bus offset]} t]
  (= 0 (mod (+ t offset) bus)))

(defn valid? [t buses]
  (->> buses
       (map #(valid-bus? % t))
       (every? true?)))

(defn less-brute [f]
  (let [buses (buses-with-offsets f)]
    (loop [t 0]
      (if (valid? t buses)
        t
        (recur (+ t 1))))))

(comment
  (less-brute "example.txt")
  (less-brute "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; borrowed from rschmukler
;; this took me forever to read
;; the use of `iterate` was new to me
;; - it accumulates lcm for each bus+offset
;; that way each new time `t` is already valid for all previous buses,
;; and the incremented amount grows with each new factor added

(defn step-time
  [time-gen bus]
  (let [step (->> time-gen (take 2) reverse (apply -))]
    (println "bus" bus)
    (println "time-gen" (->> time-gen (take 5)))
    (println "step" step)
    (->> time-gen
         (filter (partial valid-bus? bus))
         first
         (iterate #(+ % (lcm step (:bus bus)))))))

(comment
  (->> (buses-with-offsets "example.txt")
       (reduce step-time (range))
       first)

  (->> (buses-with-offsets "input.txt")
       (reduce step-time (range))
       first)
  )
