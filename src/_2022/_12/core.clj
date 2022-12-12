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
  [grid-data path]
  (let [[x y]             (:current path)
        goal              (:goal grid-data)
        grid              (:grid grid-data)
        current-elevation (if (#{[x y]} (:start grid-data))
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
                        (#{e (dec e)} current-elevation)

                        ;; we can go down too....
                        (and
                          (not (#{\E} ele))
                          (> current-elevation e))

                        (and (#{(int \z)} current-elevation)
                             (#{goal} pos)))))))))))

(comment
  (def grid-data (input "example.txt"))
  (neighbors grid-data (init-path grid-data))

  (def grid-data-2 (input "input.txt"))
  (neighbors grid-data-2 (init-path grid-data-2)))

(defn remove-redundant-paths [grid-data paths]
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

    ;; (when (< 4 (count shortest))
    ;;   (->> paths (map #(assoc % :grid-data grid-data)) (map draw-path) doall)
    ;;   (->> paths (map :current) println)
    ;;   (println shortest))
    shortest))

(defn extend-path [grid-data path point]
  (-> path
      (update :current-elevation #(max (or % 0) (int ((:grid grid-data) point))))
      (assoc :current point)
      (update :steps conj point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-shortest-path [f]
  (let [grid-data (input f)
        goal      (:goal grid-data)
        debug     false]
    (loop [n          0
           paths      [(init-path grid-data)]
           last-paths nil]
      (when debug
        (println "\n----------------------- " n " ----------------------"
                 (->> paths (map :current))))
      (let [paths    (remove-redundant-paths grid-data paths)
            complete (some->> paths (filter :complete) (sort-by (comp count :steps)) first)]
        (if (or (and debug (> n 3)) (= last-paths paths))
          (->> paths (map #(assoc % :grid-data grid-data)))
          (if complete (assoc complete :grid-data grid-data)
              (let [updated-paths
                    (->> paths
                         (mapcat (fn [p]
                                   (let [nbrs (neighbors grid-data p)]
                                     ;; TODO prefer more elevated nbrs
                                     (->> nbrs
                                          (map (fn [nbr]
                                                 (cond-> (extend-path grid-data p nbr)
                                                   (#{goal} nbr)
                                                   (assoc :complete true)))))))))
                    ;; pth-ct        (count updated-paths)
                    ;; ;; drop lower-elevation paths
                    ;; updated-paths (->>
                    ;;                 updated-paths
                    ;;                 (group-by :current-elevation)
                    ;;                 (sort-by first >)
                    ;;                 vals
                    ;;                 first)
                    ;; new-pth-ct    (count updated-paths)
                    ]
                ;; (when-not (#{pth-ct} new-pth-ct)
                ;;   (println "Dropped unelevated paths from" pth-ct "to" new-pth-ct
                ;;            (-> updated-paths first :current-elevation)))

                (recur (inc n) updated-paths paths))))))))

(comment
  (int \m)

  (find-shortest-path "example.txt")
  (find-shortest-path "input.txt")

  (def shortest *1)
  (draw-path shortest)
  (-> shortest :steps count dec)

  (->> *1 (map draw-path))
  (draw-path *1)

  (-> *1 :steps count dec)
  )
