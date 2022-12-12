(ns _2022._12.core
  (:require
   [util :as util]
   [clojure.string :as string]))

(defn path [f]
  (str "src/_2022/_12/" f))

(defn grid [f]
  (-> f
      path
      (util/parse-input)
      (->> (map-indexed (fn [y row]
                          (println row)
                          (->> row
                               (map-indexed (fn [x ele]
                                              [[x y] ele])))
                          ))
           (apply concat)
           (into {})
           )))

(comment
  (grid "example.txt")
  (grid "input.txt")
  )
