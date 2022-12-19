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
        {:ore-robot      {:ore ore-robot-ore-cost}
         :clay-robot     {:ore clay-robot-ore-cost}
         :obsidian-robot {:ore  obsidian-robot-ore-cost
                          :clay obsidian-robot-clay-cost}
         :geode-robot    {:ore      geode-robot-ore-cost
                          :obsidian geode-robot-obsidian-cost}}}))
    init-bp))

(defn input [f]
  (-> f f-path util/parse-input (->> (map parse-blueprint))))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn build-robots [bp]
  ;; TODO should we spend our minerals, or not?
  (println "minerals to spend" (:minerals bp))
  (println "costs" (:costs bp))
  bp)

(defn simulate-robots
  ([bp] (simulate-robots bp (:robots bp)))
  ([bp robots]
   (reduce
     (fn [bp [bot-type count]]
       (update-in bp [:minerals bot-type] + count))
     bp robots)))

(comment
  (-> (first (input "example.txt"))
      (assoc-in [:robots :clay] 2)
      (assoc-in [:minerals :clay] 2)
      simulate-robots))

(defn sim-bp-one-minute [bp]
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
  (->
    (input "example.txt")
    second
    (simulate-blueprint 3)))

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
