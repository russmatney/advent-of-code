(ns _2022._19.core
  (:require [util :as util]))

(defn f-path [f]
  (str "src/_2022/_19/" f))

(defn init-bp [bp]
  (merge
    bp
    {:minerals
     {:ore      0
      :clay     0
      :obsidian 0
      :geode    0}
     :robots
     {:ore      1
      :clay     0
      :obsidian 0
      :geode    0}}))

(defn parse-blueprint [line]
  (->
    (re-seq #"(\d+)" line)
    (->> (map first) (map read-string))
    ((fn [[id ore-robot-ore-cost clay-robot-ore-cost
           obsidian-robot-ore-cost obsidian-robot-clay-cost
           geode-robot-ore-cost geode-robot-obsidian-cost]]
       {:blueprint-id id
        :costs
        {:ore      {:ore ore-robot-ore-cost}
         :clay     {:ore clay-robot-ore-cost}
         :obsidian {:ore  obsidian-robot-ore-cost
                    :clay obsidian-robot-clay-cost}
         :geode    {:ore      geode-robot-ore-cost
                    :obsidian geode-robot-obsidian-cost}}}))
    init-bp))

(defn input [f]
  (-> f f-path util/parse-input (->> (map parse-blueprint))))

(def ex-1 (-> (input "example.txt") first))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn apply-cost
  "Returns nil if there are not enough minerals for the cost."
  [minerals cost]
  (reduce
    (fn [mins [min-type need]]
      (when mins
        (let [have (mins min-type)]
          (when (>= have need)
            (update mins min-type - need)))))
    minerals cost))

(comment
  (-> ex-1
      ((fn [{:keys [minerals costs]}]
         (apply-cost
           (assoc minerals :ore 2)
           (:clay costs))))))

(defn robot-build-options
  "We can only build one robot at a time, so this just
  returns robot types that we can afford along with the new
  minerals value after building it."
  [bp]
  (let [minerals (:minerals bp)
        costs    (:costs bp)]
    (->> costs
         (map (fn [[r-type cost]]
                [r-type (apply-cost minerals cost)]))
         (remove (comp nil? second)))))

(comment
  (robot-build-options
    (assoc-in ex-1 [:minerals :ore] 2)))

;; TODO should we spend our minerals, or not?
;; TODO maybe some kind of next-robot algo?

(defn build-robots [bp]
  (println "minerals" (:minerals bp))
  (println "robots" (:robots bp))
  (let [build-opts (robot-build-options bp)]
    (when (seq build-opts)
      (println "could build: " (->> build-opts (map first))))

    ;; TODO to build or not, and which one
    (cond
      ;; strategy: build as soon as we can
      (seq build-opts)
      (->> build-opts
           ;; sort
           first ;; can only build one at a time
           ((fn [[bot-type new-minerals]]
              (println "Building" bot-type)
              (-> bp
                  (update-in [:robots bot-type] inc)
                  (assoc :minerals new-minerals)))))

      :else bp)))

(defn simulate-robots
  ([bp] (simulate-robots bp (:robots bp)))
  ([bp robots]
   (reduce (fn [bp [bot-type count]]
             (update-in bp [:minerals bot-type] + count))
           bp robots)))

(defn sim-bp-one-minute [bp]
  (println "\n")
  (-> bp
      ;; build the robots and subtract from minerals
      build-robots
      ;; simulate with existing robots (collect minerals)
      (simulate-robots (:robots bp))))

(defn simulate-blueprint [bp mins]
  (->> (iterate sim-bp-one-minute bp)
       (take (inc mins))
       last))

(comment
  (-> (input "example.txt") first ;; second
      (simulate-blueprint 3))

  (-> (input "example.txt") first ;; second
      (simulate-blueprint 9))
  )

(defn sim-all-bps [f]
  (->>
    (input f)
    (group-by :blueprint-id)
    (map (fn [[id bps]]
           [id (simulate-blueprint (first bps) 24)]))
    (into {})))

(comment
  (->>
    (sim-all-bps "example.txt")
    (map (fn [[id bp]]
           [id (-> bp :minerals :geode)]))))
