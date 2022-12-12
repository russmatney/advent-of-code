(ns _2022._12.core
  (:require
   [util :as util]))

(defn path [f]
  (str "src/_2022/_12/" f))

(defn parse-grid [f]
  (-> f
      path
      (util/parse-input)
      (->> (map-indexed
             (fn [y row] (->> row (map-indexed (fn [x ele] [[x y] ele])))))
           (apply concat)
           (into {}))))

(comment
  (parse-grid "example.txt")
  (parse-grid "input.txt"))

(defn input [f]
  (let [grid      (parse-grid f)
        start-pos (->> grid (filter (comp #{\S} second)) first first)
        end-pos   (->> grid (filter (comp #{\E} second)) first first)]
    {:grid  grid
     :start start-pos
     :goal  end-pos}))

(comment
  (input "example.txt")
  (input "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-path [{:keys [steps current grid-data]}]
  (println "\n--------------------------Drawing path-----------------")
  (let [max-x (->> steps (map first) (apply max))
        min-x (->> steps (map first) (apply min))
        max-y (->> steps (map second) (apply max))
        min-y (->> steps (map second) (apply min))
        rendered
        (->>
          (range min-y (inc max-y))
          (map (fn [y]
                 (str y ":\t"
                      (->>
                        (range min-x (inc max-x))
                        (map (fn [x] (cond
                                       (#{current} [x y]) "V"
                                       (steps [x y])      ((:grid grid-data) [x y])
                                       :else              ".")))
                        (apply str))
                      "\n")))
          (apply str))]
    (println rendered)
    rendered))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-path [input]
  {:current (:start input)
   :steps   #{(:start input)}})

(defn neighbors
  [grid-data path keep-neighbor?]
  (let [[x y] (:current path)
        grid  (:grid grid-data)]
    (->>
      {:right [(inc x) y]
       :left  [(dec x) y]
       :down  [x (dec y)]
       :up    [x (inc y)]}
      vals
      (remove (:steps path))
      (filter (fn [pos]
                (let [ele (grid pos)]
                  ;; drop nil (off grid edge)
                  (when ele (keep-neighbor? grid-data path ele))))))))

(defn remove-redundant-paths [paths]
  (let [passed (count paths)
        shortest
        (->> paths
             (group-by :current)
             (map second)
             (map (fn [paths]
                    (->> paths
                         (sort-by (comp count :steps))
                         first))))]

    (when (< (count shortest) passed)
      (println "reduced from" passed "to" (count shortest) "paths"))
    shortest))

(defn extend-path [grid-data path point]
  (-> path
      (update :current-elevation #(max (or % 0) (int ((:grid grid-data) point))))
      (assoc :current point)
      (update :steps conj point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-shortest-path [f {:keys [->init-path is-goal? keep-neighbor?]}]
  (let [grid-data (input f)
        debug     false]
    (loop [n          0
           paths      [(->init-path grid-data)]
           last-paths nil]
      (let [paths    (remove-redundant-paths paths)
            complete (some->> paths (filter :complete) (sort-by (comp count :steps)) first)]
        (if (or (and debug (> n 3)) (= last-paths paths))
          (->> paths (map #(assoc % :grid-data grid-data)))
          (if complete
            (assoc complete :grid-data grid-data)
            (let [updated-paths
                  (->> paths
                       (mapcat (fn [p]
                                 (let [nbrs (neighbors grid-data p keep-neighbor?)]
                                   (->> nbrs
                                        (map (fn [nbr]
                                               (cond-> (extend-path grid-data p nbr)
                                                 (is-goal? grid-data nbr)
                                                 (assoc :complete true)))))))))]
              (recur (inc n) updated-paths paths))))))))

(def part-1-opts
  {:->init-path    init-path
   :is-goal?       (fn [grid-data point] (#{(:goal grid-data)} point))
   :keep-neighbor? (fn [grid-data path elevation]
                     (let [current-pos       (:current path)
                           current-elevation ((:grid grid-data) current-pos)
                           ce                (int current-elevation)
                           e                 (int elevation)]
                       (cond
                         (#{\S} current-elevation) (#{\a} elevation)
                         (#{\E} elevation)         (#{\z} current-elevation)
                         :else                     (>= ce (dec e)))))})

(def part-2-opts
  {:->init-path    (fn [grid-data]
                     ;; start at goal
                     {:current (:goal grid-data)
                      :steps   #{(:goal grid-data)}})
   :is-goal?       (fn [grid-data point] (#{\a} ((:grid grid-data) point)))
   :keep-neighbor? (fn [grid-data path elevation]
                     (let [current-elevation ((:grid grid-data) (:current path))
                           ce                (int current-elevation)
                           e                 (int elevation)]
                       (if (#{\E} current-elevation)
                         (#{\z} elevation)
                         (>= e (dec ce)))))})

(comment
  (find-shortest-path "example.txt" part-1-opts)
  (find-shortest-path "input.txt" part-1-opts)

  (find-shortest-path "example.txt" part-2-opts)
  (find-shortest-path "input.txt" part-2-opts)

  (def shortest *1)
  (draw-path shortest)
  (-> shortest :steps count dec))
