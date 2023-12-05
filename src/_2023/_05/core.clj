(ns _2023._05.core
  (:require [util :as util]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_05/" fname)
                    {:partition? true}))

(defn ->seeds [data]
  (->> data first first (re-seq #"(\d+)") (map first) (map parse-long)))

(defn ->seed-map [lines]
  (->> lines rest
       (map (fn [line]
              (let [[dest src range]
                    (->> line
                         (re-seq #"(\d+)")
                         (map first)
                         (map parse-long))]
                {:src src :dest dest :rng range})))))

(defn ->seed-maps [data]
  (->> data (rest) (map ->seed-map)))

(defn map-value [map-data val]
  (let [lowest-data (->> map-data
                         (filter #(<= (:src %) val))
                         (sort-by :src >)
                         first)]
    (cond
      (nil? lowest-data)              val
      (>= val (+ (:src lowest-data)
                 (:rng lowest-data))) val
      :else
      (let [diff (- val (:src lowest-data))]
        (+ (:dest lowest-data) diff)))))

(defn seed->location [seed-maps seed]
  (reduce #(map-value %2 %1) seed seed-maps))

(defn process-mappings [data]
  (let [sms (->seed-maps data)]
    (->> data ->seeds
         (map (partial seed->location sms))
         (apply min))))

(comment
  ;; part 1
  (process-mappings (input "example.txt"))
  (process-mappings (input "input.txt")))

;; part 2

(defn ->minimal-map-inputs [[val val-rng] map-data]
  (let [rng-start val
        rng-end   (+ val val-rng)
        opts      (->> map-data
                       (sort-by :src)
                       (filter (fn [{src :src}]
                                 (and (< src rng-end) (> src rng-start))))
                       (map (fn [{:keys [src rng]}]
                              [src
                               (when (<= (+ src rng) rng-end)
                                 (+ src rng))])))]
    (->>
      (apply concat [rng-start] opts)
      dedupe
      (remove nil?)
      sort)))

(defn ->expand-range [rng map-data]
  (let [[rng-start remaining-rng] rng
        min-starts                (->minimal-map-inputs rng map-data)]
    (reduce
      (fn [ranges [curr next]]
        (conj ranges [curr (- next curr)]))
      #{}
      (->> min-starts
           (partition 2 1 [(inc (+ rng-start remaining-rng))])))))

(defn ->expand-ranges [rngs map-data]
  (reduce #(apply merge %1 (->expand-range %2 map-data)) #{} rngs))


(defn range->lowest-mapped-value [rng map-data]
  (let [inputs (->minimal-map-inputs rng map-data)]
    (->> inputs
         (map #(map-value map-data %))
         (apply min))))

(defn ranges->lowest-mapped-value [input-ranges map-data]
  (->> input-ranges
       (map #(range->lowest-mapped-value % map-data))
       (apply min)))

(defn ->ranges [data]
  (partition 2 2 (->seeds data)))

(comment
  (time
    (let [data      (input "input.txt")
          rgs       (->ranges data)
          seed-maps (->seed-maps data)
          final-map (last seed-maps)
          seed-maps (butlast seed-maps)
          final-expanded
          (reduce
            (fn [all-rngs seed-map]
              (let [expanded (->expand-ranges all-rngs seed-map)
                    mapped   (->> expanded
                                  (map (fn [[src rng]]
                                         [(map-value seed-map src) rng]))
                                  (into #{}))]
                mapped))
            (->> rgs (into #{}))
            seed-maps)
          lowest    (ranges->lowest-mapped-value final-expanded final-map)]

      (println "final lowest location" lowest)
      lowest
      ))
  )
