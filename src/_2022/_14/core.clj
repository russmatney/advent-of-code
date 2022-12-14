(ns _2022._14.core
  (:require [util :as util]))

(defn f-path [f]
  (str "src/_2022/_14/" f))

(defn paths [f]
  (-> f
      f-path
      (util/parse-input {:split #" -> "})
      (->> (map (fn [pts]
                  (->> pts
                       (map (fn [pt]
                              (->>
                                (re-seq #"(\d+)" pt)
                                (map second)
                                (map read-string)
                                vec)))))))))

(comment
  (paths "example.txt")
  (paths "input.txt"))
