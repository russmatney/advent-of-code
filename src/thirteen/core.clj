(ns thirteen.core
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

(defn parse-buses [f]
  (-> (input f)
      second
      (string/split #",")
      (->> (map (fn [val]
                  (if (#{"x"} val) nil
                      (read-string val))))
           (into []))))

(comment
  (parse-buses "input.txt")
  (parse-buses "example.txt")

  (apply max (parse-buses "example.txt"))


  (->>
    (parse-buses "example.txt")
    (apply *)
    )

  (mod (+ 4 3) 7)
  (every? true? [true false true])
  )

(defn ordered-arrivals? [t buses]
  (->> buses
       (map-indexed
         (fn [i bus]
           (if (nil? bus)
             true
             (let [t (+ i t)]
               (= 0 (mod t bus))))))
       (every? true?)))

(defn brute [buses]
  (let [largest (->> buses
                     (remove nil?)
                     (apply max))
        fst     (first buses)
        ]
    (loop [t 0]
      (if (ordered-arrivals? t buses)
        t
        (recur (+ t fst))))))

(comment
  (println "Hi")
  (let [buses (parse-buses "example.txt")]
    (brute buses))
  (let [buses (parse-buses "input.txt")]
    (println "running")
    (println (brute buses))))

(defn buses-with-offsets [f]
  (let [buses          (parse-buses f)
        with-offsets
        (->> buses
             (map-indexed
               (fn [i bus]
                 (if (nil? bus)
                   nil
                   [bus i])))
             (remove nil?)
             (into {}))
        largest        (apply max (remove nil? buses))
        largest-offset (with-offsets largest)

        with-offsets
        (->> with-offsets
             (map (fn [[bus offset]]
                    {:bus    bus
                     :offset (- offset largest-offset)})))]
    {:largest largest
     :buses   with-offsets}))

(comment
  (buses-with-offsets "example.txt")

  (mod 14 7)
  )

(defn ordered? [i buses]
  (every? true?
          (->> buses
               (map (fn [{:keys [bus offset]}]
                      (let [goal (+ i offset)]
                        (= 0 (mod goal bus))))))))

(defn less-brute [f]
  (let [{:keys [largest buses]} (buses-with-offsets f)
        first-offset-val        (->> buses (map :offset) sort first)]
    (println first-offset-val)
    (loop [i 0]
      (if (ordered? i buses)
        (+ i first-offset-val)
        (recur (+ i largest))))))

(comment
  (less-brute "example.txt")
  (less-brute "input.txt")
  )
