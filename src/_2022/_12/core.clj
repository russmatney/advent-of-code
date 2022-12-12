(ns _2022._12.core
  (:require
   [util :as util]))

(defn path [f]
  (str "src/_2022/_12/" f))

(defn parse-grid [f]
  (-> f
      path
      (util/parse-input)
      (->> (map-indexed (fn [y row]
                          (->> row
                               (map-indexed (fn [x ele]
                                              [[x y] ele])))
                          ))
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
     :goal  end-pos

     :current start-pos
     :steps   #{start-pos}}))

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

(comment
  (draw-path {:steps #{[0 0]
                       [0 1]
                       [1 0]}})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-path [input]
  {:current (:start input)
   :steps   #{(:start input)}})

(defn neighbors
  ([grid-data path]
   (let [[x y]     (:current path)
         goal      (:goal grid-data)
         grid      (:grid grid-data)
         elevation (if (#{[x y]} (:start grid-data))
                     96 ;; one less than (int \a)
                     (int (grid [x y])))]
     (->>
       {:right [(inc x) y]
        :left  [(dec x) y]
        :down  [x (dec y)]
        :up    [x (inc y)]}
       vals
       (remove (:steps path))
       (filter (fn [pos]
                 (let [ele (grid pos)]
                   ;; drop nil ele (off grid edge)
                   (when ele
                     (let [e (int ele)]
                       (or
                         ;; keep if same or elevation + 1
                         (#{e (dec e)} elevation)

                         (and (#{122} elevation)
                              (#{goal} pos))))))))))))

(comment
  (int \z)
  (def grid-data (input "example.txt"))
  (neighbors grid-data (init-path grid-data))

  (def grid-data-2 (input "input.txt"))
  (neighbors grid-data-2 (init-path grid-data-2)))

(defn remove-redundant-paths [paths]
  ;; TODO
  paths)

(defn extend-path [path point]
  (-> path
      (assoc :current point)
      (update :steps conj point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-shortest-path [f]
  (let [grid-data (input f)
        goal      (:goal grid-data)]
    (loop [paths [(init-path grid-data)]]
      (let [paths    (remove-redundant-paths paths)
            complete (some->> paths (filter :complete) first)]
        ;; (println "looping with paths" paths)
        (if complete (assoc complete :grid-data grid-data)
            (let [updated-paths
                  (->> paths
                       (mapcat (fn [p]
                                 (let [nbrs (neighbors grid-data p)]
                                   (->> nbrs
                                        (map (fn [nbr]
                                               (cond-> (extend-path p nbr)
                                                 (#{goal} nbr)
                                                 (assoc :complete true)))))))))]
              (recur updated-paths)))))))


(comment
  (find-shortest-path "example.txt")

  (find-shortest-path "input.txt")

  (draw-path *1)
  (-> *1 :steps count dec)
  )
