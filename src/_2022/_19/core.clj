(ns _2022._19.core
  (:require [util :as util]))

(defn f-path [f]
  (str "src/_2022/_19/" f))

(defn parse-blueprint [line]
  (->
    (re-seq #"(\d+)" line)
    (->> (map first) (map read-string))
    ((fn [[id ore-robot-ore-cost clay-robot-ore-cost
           obsidian-robot-ore-cost obsidian-robot-clay-cost
           geode-robot-ore-cost geode-robot-obsidian-cost]]
       {:blueprint-id   id
        :ore-robot      {:ore ore-robot-ore-cost}
        :clay-robot     {:ore clay-robot-ore-cost}
        :obsidian-robot {:ore  obsidian-robot-ore-cost
                         :clay obsidian-robot-clay-cost}
        :geode-robot    {:ore      geode-robot-ore-cost
                         :obsidian geode-robot-obsidian-cost}}))))

(defn input [f]
  (-> f f-path util/parse-input (->> (map parse-blueprint))))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn init-bp [bp]
  (merge bp
         {:ore      0
          :clay     0
          :obsidian 0
          :geode    0}))

(defn sim-bp-one-minute [bp]
  bp)

(defn simulate-blueprint [bp mins]
  bp)
