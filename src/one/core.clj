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

(comment
  (apply * (pair-that-equals 2020 (sort (input))))
  (apply * (triple-that-equals 2020 (sort (input))))

  (apply * (pair-that-equals 2020 (sort [1721 979 366 299 675 1456])))
  (apply * (triple-that-equals 2020 (sort [1721 979 366 299 675 1456])))
  )

(defn n-that-eq [n target xs]
  (->> xs
       (reduce
         (fn [{:keys [remaining ns] :as agg} x]
           (if-let [more-ns
                    (cond
                      (> n 1)      (when-let [ns (seq (n-that-eq (- n 1) (- target x) remaining))]
                                     (conj ns x))
                      (= x target) [x]
                      :else        nil)]
             (reduced (assoc agg :ns (concat more-ns ns)))
             agg))
         {:remaining (rest xs) :ns []})
       :ns))

(comment
  (n-that-eq 1 2020 (sort [1721 979 366 299 675 1456 2020]))
  (n-that-eq 2 2020 (sort [979 366 1721 299 675 1456]))
  (n-that-eq 3 2020 (sort [979 366 1721 299 675 1456]))

  (->>
    [1721 979 366 299 675 1456]
    sort
    (n-that-eq 3 2020)
    (apply *)
    )

  (->>
    (input)
    sort
    (n-that-eq 3 2020)
    (apply *)
    )
  )
