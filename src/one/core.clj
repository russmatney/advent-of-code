(ns one.core
  "
  Part One:
  Read in a list of numbers.
  Find the two that sum to `2020`
  Multiply them together.

  Part Two:
  Find 3 in the list that total 2020, and multiply them.
  "
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))

(defn pair-that-equals [target sorted]
  (let [reversed (reverse sorted)]
    (->> reversed
         (keep
           (fn [x]
             (when-let [y (some->> sorted
                                   (filter #(= % (- target x)))
                                   first)]
               [x y])))
         first)))

(defn triple-that-equals [target sorted]
  (let [reversed (reverse sorted)]
    (->> reversed
         (keep
           (fn [x]
             (when-let [[y z] (pair-that-equals (- target x) (remove #(> % x) sorted))]
               [x y z])))
         first)))

(defn input []
  (-> *file*
      io/file
      .getParent
      (str "/input.txt")
      slurp
      string/split-lines
      (#(map read-string %))))

(defn run [f raw]
  (->> raw
       sort
       (f 2020)
       (apply *)
       ))

(comment
  (run pair-that-equals (input))
  (run triple-that-equals (input))


  (run pair-that-equals [1721 979 366 299 675 1456])
  (run triple-that-equals [1721 979 366 299 675 1456])

  )
